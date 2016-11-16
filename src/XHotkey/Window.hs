module XHotkey.Window where

import XHotkey.Types
import XHotkey.Core
import XHotkey.Window.Internal

import Graphics.X11
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Graphics.X11.Xlib.Extras

import Control.Concurrent
import Control.Monad.Reader

import Data.Bits
import Data.IORef

import Foreign
import Foreign.C

import qualified Data.Map as M

type EventCB = Event -> XWin ()

data WinEnv = WinEnv
    { win_dpy       :: Display
    , win_id        :: Window
    , win_attr      :: Ptr SetWindowAttributes
    , win_gc        :: GC
    , win_strbounds :: String -> XWin (Dimension, Dimension, Dimension)
    , win_putstr    :: Position -> Position -> String -> XWin ()
    , win_evmap     :: IORef (M.Map EventType EventCB)
    , win_kill      :: Bool
    }

data WinRes = WinRes
    { res_bordersize    :: CInt
    , res_bordercolor   :: Pixel
    , res_bgcolor       :: Pixel
    , res_fgcolor       :: Pixel
    , res_font          :: String
    }

type XWin = ReaderT WinEnv IO

putStrTL :: Position -> Position -> String -> XWin ()
putStrTL x y str = do
    wenv <- ask
    (asc, _, _) <- (win_strbounds wenv) str
    (win_putstr wenv) x (y- (fromIntegral asc)) str

setEventCB :: EventType -> EventCB -> XWin ()
setEventCB typ fun = do
    WinEnv { win_evmap = evmap } <- ask
    io $ modifyIORef' evmap (M.insert typ fun)

hideWin :: EventCB
hideWin _ = do
    WinEnv { win_dpy = dpy, win_id = wid } <- ask
    io $ unmapWindow dpy wid
    io $ flush dpy 

type WinChan = MChan XWin 

newWinChan :: MonadIO m => m (WinChan a)
newWinChan = newMChan

closeWinChan :: MonadIO m => WinChan a -> m ()
closeWinChan = closeMChan

evalWinChan :: MonadIO m => WinChan a -> XWin a -> m a
evalWinChan = evalMChan

runWinChan :: MonadIO m => WinChan a -> (XWin a -> m a) -> m Bool
runWinChan = runMChan

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

parWin :: WinRes -> X (WinChan a)
parWin res = do
    x <- newWinChan
    forkX_ $ win res $ dowhile $ runWinChan x id
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

parMsgbox :: WinRes -> X (WinChan ())
parMsgbox res = do
    xc <- parWin res
    evalWinChan xc $ do
        WinEnv { win_dpy = dpy, win_id = wid } <- ask
        setEventCB buttonPress hideWin
        io $ unmapWindow dpy wid
        io $ flush dpy
    return xc

writeMsg :: MonadIO m => WinChan () -> String -> m ()
writeMsg xc "" = do
    evalWinChan xc $ do
        WinEnv { win_dpy = dpy, win_id = wid } <- ask
        io $ do
            unmapWindow dpy wid
            flush dpy
    return () 
writeMsg xc str = do
    evalWinChan xc $ do
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
