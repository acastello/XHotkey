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

data WinEnv = WinEnv
    { win_dpy   :: Display
    , win_id    :: Window
    , win_attr  :: Ptr SetWindowAttributes
    , win_gc    :: GC
    , win_strbounds :: String -> XWin (Dimension, Dimension, Dimension)
    , win_strdraw   :: Position -> Position -> String -> XWin ()
    , win_evf   :: IORef ( Event -> XWin () )
    , win_kill  :: Bool
    }

data WinRes = WinRes
    { res_bordersize    :: CInt
    , res_bordercolor   :: Pixel
    , res_bgcolor       :: Pixel
    , res_fgcolor       :: Pixel
    , res_font          :: String
    }

type XWin a = ReaderT WinEnv IO a

data XChan a = XChan (MVar (Either () (XWin a))) (MVar a)

newXChan :: MonadIO m => m (XChan a)
newXChan = io $ do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    return (XChan mv1 mv2)

closeXChan :: MonadIO m => XChan a -> m ()
closeXChan (XChan v1 _) = io $ do
    putMVar v1 (Left ())

evalXChan :: MonadIO m => XChan a -> XWin a -> m a
evalXChan (XChan v1 v2) action = io $ do
    putMVar v1 (Right action)
    takeMVar v2 

runXChan :: MonadIO m => XChan a -> (XWin a -> m a) -> m Bool
runXChan (XChan mv1 mv2) handle = do
    par <- io $ takeMVar mv1
    case par of
        Left _ -> return False
        Right par' -> do
            res <- handle par'
            io $ putMVar mv2 res
            return True

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

    selectInput dpy window (exposureMask .|. structureNotifyMask)

    xftdraw <- xftDrawCreate dpy window visual colormap
    xftfont <- xftFontOpen dpy (screenOfDisplay dpy screenn) fontn
    let df x y str = io $ xftDrawString xftdraw xftcol xftfont x y str
        strb str = io $ do
            asc <- fromIntegral <$> xftfont_ascent xftfont
            desc <- fromIntegral <$> xftfont_descent xftfont
            width <- fromIntegral <$> xglyphinfo_width <$> xftTextExtents dpy xftfont str 
            return (asc, desc, width)

    ev_f <- newIORef $ \ev -> do
        return ()
    let env = WinEnv dpy window attr gc strb df ev_f False
    mv <- newEmptyMVar :: IO (MVar ())
    mapWindow dpy window
    flush dpy
    proc <- io $ forkIO $ allocaXEvent $ \ev -> flip runReaderT env $ do
        dowhile $ do
            io $ nextEvent dpy ev
            ev' <- io $ getEvent ev
            (io $ readIORef ev_f) >>= ($ ev')
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
    WinEnv { win_dpy = dpy, win_id = w', win_gc = gc, win_attr = attr, win_strbounds = strb, win_strdraw = drf } <- ask
    (asc,des,wdt) <- strb str
    (_,_,hpad) <- strb "_"
    let vpad = des
    when (wdt+2*hpad > 1) $ io $ do
        resizeWindow dpy w' (wdt+2*hpad) (2*vpad+asc+des)
    drf (fromIntegral hpad) (fromIntegral $ asc + vpad) str
    io $ flush dpy
    io $ getLine
    return ()

parMsgbox :: WinRes -> X (XChan String)
parMsgbox res = do
    parWin res

writeMsg :: MonadIO m => XChan String -> m ()
writeMsg xc = do
    return ()


dowhile :: Monad m => m Bool -> m ()
dowhile act = do
    rep <- act
    if rep then
        dowhile act
    else
        return ()
