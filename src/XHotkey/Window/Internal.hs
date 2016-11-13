module XHotkey.Window.Internal where

import XHotkey.Types
import XHotkey.Core

import Graphics.X11
import Graphics.X11.Xlib.Extras

import Data.Bits
import Data.IORef

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

import Foreign

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

win :: XWin a -> X a
win act = (io initThreads >>) $ copyX $ do
    XEnv { display = dpy, rootWindow' = root } <- ask
    io $ withallocaSetWindowAttributes $ \attr -> do
        set_background_pixel attr 0x222222
        set_override_redirect attr True
        set_event_mask attr (maxBound)
        window <- createWindow dpy root 1 1 10 10 0 copyFromParent inputOutput 
            (defaultVisual dpy (defaultScreen dpy)) (cWBackPixel .|. cWOverrideRedirect) attr
        gc <- createGC dpy window
        setForeground dpy gc 0xbabdb6

        selectInput dpy window (exposureMask .|. structureNotifyMask)

        fs <- loadQueryFont dpy "9x15bold"
        let fo = fontFromFontStruct fs
            df x y str = io $ drawString dpy window gc x y str 
            strb str = return (fromIntegral $ ascentFromFontStruct fs, fromIntegral $ descentFromFontStruct fs, fromIntegral $ textWidth fs str)
        setFont dpy gc fo

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

msgbox :: String -> X ()
msgbox str = win $ do
    WinEnv { win_dpy = dpy, win_id = w', win_gc = gc, win_attr = attr, win_strbounds = strb, win_strdraw = drf } <- ask
    (asc,des,wdt) <- strb str
    (_,_,hpad) <- strb " "
    let vpad = des
    io $ do
        setWindowBorderWidth dpy w' 2
        setWindowBorder dpy w' 0xbabdb6
        resizeWindow dpy w' (wdt+2*hpad) (2*vpad+asc+des)
    drf (fromIntegral hpad) (fromIntegral $ asc + vpad) str
    io $ flush dpy
    io $ getLine
    return ()


dowhile :: Monad m => m Bool -> m ()
dowhile act = do
    rep <- act
    if rep then
        dowhile act
    else
        return ()
