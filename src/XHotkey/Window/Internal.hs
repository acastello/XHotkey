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

win' :: X Window
win' = do
    XEnv { display = dpy, rootWindow' = root } <- ask
    mvar <- io $ newEmptyMVar
    forkX $ copyX $ do
        XEnv { display = dpy', rootWindow' = root' } <- ask
        win <- io $ createSimpleWindow dpy' root' 0 0 100 100 0 0 maxBound 
        fs <- io $ loadQueryFont dpy' "6x13bold"
        let font = fontFromFontStruct fs
        gc <- io $ createGC dpy' win
        io $ do
            setFont dpy' gc font
            setForeground dpy' gc 0xff0000
            mapWindow dpy' =<< createSimpleWindow dpy' win 10 10 20 20 0 maxBound 0
            mapWindow dpy' win
            drawString dpy' win gc 5 5 "asd"
        flushX
        io $ putMVar mvar win
        io $ threadDelay 1000000
        io $ drawString dpy' win gc 0 (ascentFromFontStruct fs) "asd"
        flushX
        io $ threadDelay 10000000
    io $ takeMVar mvar

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

msgbox :: String -> X ()
msgbox str = win (WinRes 0 0xbabdb6 0x222222 0xbabdb6 "Inconsolata: pixelsize=30px") $ do
    WinEnv { win_dpy = dpy, win_id = w', win_gc = gc, win_attr = attr, win_strbounds = strb, win_strdraw = drf } <- ask
    (asc,des,wdt) <- strb str
    (_,_,hpad) <- strb "_"
    io $ print (wdt,hpad)
    let vpad = des
    when (wdt+2*hpad > 1) $ io $ do
        resizeWindow dpy w' (wdt+2*hpad) (2*vpad+asc+des)
    drf (fromIntegral hpad-1) (fromIntegral $ asc + vpad) str
    io $ flush dpy
    io $ print =<< getGeometry dpy w'
    io $ getLine
    return ()


dowhile :: Monad m => m Bool -> m ()
dowhile act = do
    rep <- act
    if rep then
        dowhile act
    else
        return ()
