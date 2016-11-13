import XHotkey.Types
import XHotkey.Core

import Graphics.X11

import Control.Concurrent
import Control.Monad.Reader

data XWin = XWin Window

win :: X Window
win = do
    XEnv { display = dpy, rootWindow' = root } <- ask
    mvar <- io $ newEmptyMVar
    f <- copyX
    io $ forkIO $ f $ do
        XEnv { display = dpy', rootWindow' = root' } <- ask
        win <- io $ createSimpleWindow dpy' root' 0 0 100 100 0 0 maxBound 
        fs <- io $ loadQueryFont dpy' "6x13bold"
        let font = fontFromFontStruct fs
        gc <- io $ createGC dpy' win
        io $ setFont dpy' gc font
        io $ setForeground dpy' gc 0xff0000
        io $ mapWindow dpy' =<< createSimpleWindow dpy' win 10 10 20 20 0 maxBound 0
        io $ mapWindow dpy' win
        io $ drawString dpy' win gc 5 5 "asd"
        flushX
        io $ putMVar mvar win
        io $ threadDelay 1000000
        io $ drawString dpy' win gc 0 (ascentFromFontStruct fs) "asd"
        flushX
        io $ threadDelay 10000000
    io $ takeMVar mvar
