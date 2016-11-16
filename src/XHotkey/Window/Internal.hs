module XHotkey.Window.Internal where

import XHotkey.Types
import XHotkey.Core

import Graphics.X11
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Graphics.X11.Xlib.Extras

import Data.Bits
import Data.IORef

import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Foreign
import Foreign.C

import qualified Data.Map as M


data MChan m a = MChan_ (MVar (Either () (m a))) (MVar a) (IORef Bool)
type XChan = MChan XWin 

newMChan :: (MonadIO m, MonadIO m') => m' (MChan m a)
newMChan = io $ do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    running <- newIORef True
    return (MChan_ mv1 mv2 running)

closeMChan :: (MonadIO m, MonadIO m') => MChan m a -> m' ()
closeMChan (MChan_ v1 _ st) = io $ do
    running <- readIORef st
    if running then do
        putMVar v1 (Left ())
        writeIORef st False
    else do
        tryPutMVar v1 (Left ()) 
        return ()

evalMChan :: (MonadIO m, MonadIO m') => MChan m a -> m a -> m' a
evalMChan (MChan v1 v2 st) action = io $ do
    running <- readIORef st
    if running then do
        putMVar v1 (Right action)
        takeMVar v2 
    else
        error "MChan is closed."

runMChan :: (MonadIO m, MonadIO m') => MChan m a -> (m a -> m' a) -> m' Bool
runMChan (MChan mv1 mv2 st) handle = do
    running <- io $ readIORef st
    if running then do
        par <- io $ takeMVar mv1
        case par of
            Left _ -> return False
            Right par' -> do
                res <- handle par'
                io $ putMVar mv2 res
                return True
    else
        return False

newXChan :: MonadIO m => m (XChan a)
newXChan = newMChan

closeXChan :: MonadIO m => XChan a -> m ()
closeXChan = closeMChan

evalXChan :: MonadIO m => XChan a -> XWin a -> m a
evalXChan = evalMChan

runXChan :: MonadIO m => XChan a -> (XWin a -> m a) -> m Bool
runXChan = runMChan

win :: WinRes -> XWin a -> X a
win (WinRes bordersz bordercol bgcolor fgcolor fontn) act = (io initThreads >>) $ copyX $ do
    XEnv { display = dpy, rootWindow' = root } <- ask
    let screenn = defaultScreen dpy
        visual = defaultVisual dpy screenn
        colormap = defaultColormap dpy screenn
    io $ 
        withXftColorValue dpy visual colormap xrc $ \xftcol ->
        allocaSetWindowAttributes $ \attr -> do

    set_background_pixel attr bgcolor
    set_override_redirect attr True
    set_border_pixel attr bordercol

    window <- createWindow dpy root 10 10 10 10 bordersz copyFromParent inputOutput 
        (defaultVisual dpy (defaultScreen dpy)) (cWBorderPixel .|. cWBackPixel .|. cWOverrideRedirect) attr
    gc <- createGC dpy window
    setForeground dpy gc fgcolor

    selectInput dpy window (exposureMask .|. structureNotifyMask .|. buttonPressMask)

    xftdraw <- xftDrawCreate dpy window visual colormap
    xftfont <- xftFontOpen dpy (screenOfDisplay dpy screenn) fontn
    let df x y str = io $ xftDrawString xftdraw xftcol xftfont x y str
        strb str = io $ do
            asc <- fromIntegral <$> xftfont_ascent xftfont
            desc <- fromIntegral <$> xftfont_descent xftfont
            width <- fromIntegral <$> xglyphinfo_width <$> xftTextExtents dpy xftfont (str)
            return (asc, desc, width)

    ev_f <- newIORef mempty
    let env = WinEnv dpy window attr gc strb df ev_f False
    mv <- newEmptyMVar :: IO (MVar ())
    mapWindow dpy window
    flush dpy
    proc <- io $ forkIO $ allocaXEvent $ \ev -> flip runReaderT env $ do
        dowhile $ do
            io $ nextEvent dpy ev
            ev' <- io $ getEvent ev
            f <- io $ readIORef ev_f
            maybe (return ()) ($ ev') $ M.lookup (ev_event_type ev') f 
            return (ev_event_type ev' /= destroyNotify)
        io $ putMVar mv ()

    ret <- runReaderT act env 
    io $ destroyWindow dpy window
    flush dpy
    io $ takeMVar mv
    return ret
    where 
        xrc = XRenderColor (fromIntegral $ fgcolor `shiftR` 16 .&. 0xff * 0x101) 
            (fromIntegral $ fgcolor `shiftR` 8 .&. 0xff * 0x101) 
            (fromIntegral $ fgcolor .&. 0xff * 0x101) 0xffff

parWin :: WinRes -> X (XChan a)
parWin res = do
    x <- newXChan
    forkX_ $ win res $ dowhile $ runXChan x id
    return x

msgbox :: String -> X ()
msgbox str = win (WinRes 2 0xbabdb6 0x222222 0xbabdb6 "Inconsolata: bold: pixelsize=15px") $ do
    WinEnv { win_dpy = dpy, win_id = w', win_gc = gc, win_attr = attr, win_strbounds = strb, win_putstr = drf } <- ask
    (asc,des,wdt) <- strb str
    (_,_,hpad) <- strb "_"
    let vpad = des
    when (wdt+2*hpad > 1) $ io $ do
        resizeWindow dpy w' (wdt+2*hpad) (2*vpad+asc+des)
    drf (fromIntegral hpad) (fromIntegral $ asc + vpad) str
    io $ flush dpy
    io $ getLine
    return ()

parMsgbox :: WinRes -> X (XChan ())
parMsgbox res = do
    xc <- parWin res
    evalXChan xc $ do
        WinEnv { win_dpy = dpy, win_id = wid } <- ask
        setEventCB buttonPress hideWin
        io $ unmapWindow dpy wid
        io $ flush dpy
    return xc

writeMsg :: MonadIO m => XChan () -> String -> m ()
writeMsg xc "" = do
    evalXChan xc $ do
        WinEnv { win_dpy = dpy, win_id = wid } <- ask
        io $ do
            unmapWindow dpy wid
            flush dpy
    return () 
writeMsg xc str = do
    evalXChan xc $ do
        WinEnv { win_dpy = dpy, win_putstr = putstr, win_strbounds = strext, win_id = wid } <- ask
        (asc, des, width) <- strext str
        io $ do
            mapWindow dpy wid
            resizeWindow dpy wid (width+2) (asc + des + 2)
            clearWindow dpy wid
            flush dpy
        putstr 1 (1 + fromIntegral asc) str
        io $ flush dpy

dowhile :: Monad m => m Bool -> m ()
dowhile act = do
    rep <- act
    if rep then
        dowhile act
    else
        return ()
