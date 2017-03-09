{-# LANGUAGE DoAndIfThenElse, OverloadedStrings, BangPatterns #-}

module XHotkey.Core where

import XHotkey.Types
import Data.NMap

import Graphics.X11
import Graphics.X11.Xlib.Extras
import System.Posix.Process
import System.Posix.Types

import Data.IORef
import Data.Word

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader 

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Tree as T

import Foreign hiding (void)
import Foreign.C

import Prelude hiding (lookup)

import System.Process (callCommand)

withX :: X a -> XEnv -> XControl -> IO (a, XControl)
withX (X a) env control = runStateT (runReaderT a env) control

runX :: (X a) -> IO a
runX m = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    ret <- allocaXEvent $ \e -> allocaXEvent $ \e' -> try $ 
        fillBytes e 0 xeventsize >> withX m (XEnv dpy root e e') 
        defaultXControl { xtargets = [root] }
    closeDisplay dpy
    case ret of
        Left e -> error $ show (e :: SomeException)
        Right (ret', _) -> return ret'

runForkedX :: X () -> IO ForkedX
runForkedX act = do
    initThreads
    mvar <- newEmptyMVar
    thread <- forkIO $ runX $ do
        xenv <- ask
        w <- io $ createSimpleWindow (xdisplay xenv) (xroot xenv) 0 0 1 1 0 0 0
        io $ putMVar mvar (xenv, w)
        act
    (env, win) <- takeMVar mvar
    return (env,win,thread)

queueX :: ForkedX -> X () -> IO ()
queueX (XEnv { xdisplay = dpy } , win, _) act = do
    msg <- newClientMessage dpy win act
    io $ do
        sendEvent dpy win False 0 msg
        -- putBackEvent dpy c
        -- sendClientMessage dpy c
        flush dpy
        free msg

exitForkedX :: ForkedX -> IO ()
exitForkedX = flip queueX exitX

killForkedX :: ForkedX -> IO ()
killForkedX fx @ (_,_,tid) = do
    queueX fx exitX
    killThread tid

copyX :: X a -> X a
copyX act = do
    xenv @ XEnv { xdisplay = dpy, xroot = root } <- ask
    xctrl <- get
    io $ allocaXEvent $ \ev' -> allocaXEvent $ \ev'' -> do
        fillBytes ev' 0 xeventsize
        dpy' <- openDisplay (displayString dpy)
        let root' = defaultRootWindow dpy'
        ret <- withX act (XEnv dpy' root' ev' ev'') xctrl >>= return . fst
        closeDisplay dpy'
        return ret

attachTo :: Window -> Window -> Int32 -> Int32 -> X ()
attachTo ch pa x y = do
    XEnv { xdisplay = dpy } <- ask
    liftIO $ reparentWindow dpy ch pa x y
        

waitX :: X ()
waitX = do
    return ()

setBindings :: Bindings -> X ()
setBindings binds = do
    xctrl <- get
    let oldbase = rootKeys (xbindings xctrl)
        newbase = rootKeys binds
    mapM_ _grabKM (newbase L.\\ oldbase)
    mapM_ _ungrabKM (oldbase L.\\ newbase)
    put xctrl { xbindings = binds }

mainLoop :: X ()
mainLoop = do
    s@XControl { xbindings = hk } <- get
    hk2 <- traverseKeys normalizeKM hk
    put $ s { xbindings = hk2 }
    tmp <- io $ callocXEvent
    loop tmp mempty
    io $ free tmp
    return ()
    where 
      loop :: XEventPtr -> [KM] -> X ()
      loop tmp hk = do
        -- printBindings
        xc @ XControl { xbindings = hk' } <- get
        mapM_ _grabKM (rootKeys hk' L.\\ hk)
        mapM_ _ungrabKM (hk L.\\ (rootKeys hk'))
        XEnv { xdisplay = dpy, xroot = root, xlastevent = ptr } <- ask
        XControl { xbindings = hk' } <- get
        liftIO $ nextEvent dpy ptr
        typ <- io $ get_EventType ptr
        if typ == clientMessage then
            join $ io $ consumeClientMessage ptr
        else
            step tmp 
        loop tmp (rootKeys hk')
      step :: XEventPtr -> X ()
      step tmp = do
        XControl { xbindings = hk' } <- get
        XEnv { xdisplay = dpy, xroot = root, xlastevent = ptr } <- ask
        nevs <- io $ eventsQueued dpy queuedAfterReading
        km <- ptrToKM tmp nevs
        -- io $ print km
        case do
            km' <- km
            x <- lookup km' hk'
            case x of
                Left m -> return $ do
                    liftIO $ do 
                        grabKeyboard dpy root True grabModeAsync grabModeAsync currentTime
                        grabPointer dpy root True (buttonPressMask .|. buttonReleaseMask) grabModeAsync grabModeAsync 0 0 0 
                        -- maskEvent dpy (buttonPressMask .|. buttonReleaseMask .|. keyPressMask .|. keyReleaseMask) ptr
                    x <- grabbedLoop tmp m
                    liftIO $ do 
                        ungrabKeyboard dpy currentTime
                        ungrabPointer dpy currentTime
                    x
                    return ()
                Right x -> return x
            of
            Just x -> x 
            Nothing -> return ()
                
      ptrToKM :: XEventPtr -> CInt -> X (Maybe KM)
      ptrToKM ptr n = do
        XEnv { xdisplay = dpy, xlastevent = cur } <- ask
        (t0, km) <- io $ eventToKM' cur
        -- io $ print km
        (t1, km') <- 
            if n > 0 then io $ do
                peekEvent dpy ptr 
                eventToKM' ptr
            else
                return (0,nullKM)
        -- io $ putStrLn $ (show n) ++ ": " ++ (show km) ++ " - " ++ (show km')
        if (km == nullKM) then    
            return Nothing
        else if n > 0 && t1 == t0 && mainKey km == mainKey km' then io $ do
            -- putStrLn "skippin'"
            -- nextEvent dpy ptr
            return Nothing
        else
            return (Just km)
      grabbedLoop :: XEventPtr -> Bindings -> X (X ())
      grabbedLoop tmp m = do
        XEnv { xdisplay = dpy, xroot = root, xlastevent = ptr } <- ask
        liftIO $ nextEvent dpy ptr
        nevs <- io $ eventsQueued dpy queuedAfterReading
        km <- ptrToKM tmp nevs
        case do
            km' <- km
            x <- lookup km' m
            case x of
                Left m' -> return $ grabbedLoop tmp m'
                Right x -> return (return x)
            of
            Just x -> x
            Nothing -> return (return ())
      isKCode :: KM -> Bool
      isKCode (KM _ _ (KCode _)) = True
      isKCode _ = False
      isMButton :: KM -> Bool
      isMButton (KM _ _ (MButton _)) = True
      isMButton _ = False
        
            
    
_grabKM :: KM -> X ()
_grabKM k = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    k @ (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ grabKey dpy c st root False grabModeAsync grabModeAsync
        MButton b -> liftIO $ grabButton dpy b st root False (buttonPressMask .|. buttonReleaseMask) grabModeAsync grabModeAsync root 0

_ungrabKM :: KM -> X ()
_ungrabKM k = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ ungrabKey dpy c st root
        MButton b -> liftIO $ ungrabButton dpy b st root
    return ()

forkX :: X () -> X ThreadId
forkX x = do
    xenv@XEnv { xlastevent = ptr } <- ask
    xctrl <- get
    io $ forkIO $ allocaXEvent $ \ptr' -> do
        copyBytes ptr' ptr 196 
        withX x xenv { xlastevent = ptr' } xctrl >> return ()

forkX_ :: X a -> X ()
forkX_ = void . forkX . void

forkP :: MonadIO m => IO () -> m ProcessID
forkP prog = io . forkProcess $ do
    createSession
    prog

forkP_ :: MonadIO m => IO () -> m ()
forkP_ = (>> return ()) . forkP

shell :: String -> IO ()
shell = callCommand

spawnPID :: MonadIO m => String -> m ProcessID
spawnPID prog = forkP $ executeFile "/bin/sh" False ["-c", prog] Nothing

spawn :: MonadIO m => String -> m ()
spawn = void . spawnPID 

xlasteventType :: X EventType
xlasteventType = ask >>= liftIO . get_EventType . xlastevent

flushX :: X ()
flushX = do
    XEnv { xdisplay = dpy } <- ask
    liftIO $flush dpy

windowsTree :: X (T.Tree Window)
windowsTree = do
    XEnv { xroot = root } <- ask
    windowsTree' root
    where
        windowsTree' :: Window -> X (T.Tree Window)
        windowsTree' w = do
            dpy <- return . xdisplay =<< ask
            wins <- io $ return . (\(_,_,y) -> y) =<< queryTree dpy w 
            wins' <- traverse windowsTree' wins
            return $ T.Node w wins'
    
-- | Wait for a KeyPress, KeyRelease, ButtonPress or ButtonRelease, execute any
-- ClientMessage if necessary, will throw an exception if 
waitKM :: Bool -> X KM
waitKM acceptsRepeats = do
    xenv @ XEnv { xdisplay = dpy, xroot = root, xlastevent = ptr } <- ask
    liftIO $ nextEvent dpy ptr
    typ <- io $ get_EventType ptr
    if any (typ ==) [keyPress, keyRelease, buttonPress, buttonRelease] then do
        (t,km) <- io $ eventToKM' ptr
        if acceptsRepeats then
            return km
        else do
            nevs <- io $ eventsQueued dpy queuedAfterReading
            if nevs > 0 then do
                (t', km') <- io $ do
                    peekEvent dpy ptr
                    eventToKM' ptr
                if t == t' && mainKey km == mainKey km' then do
                    io $ nextEvent dpy ptr
                    waitKM acceptsRepeats
                else
                    return km
            else
                return km
    else do
        when (typ == clientMessage) $
            join $ io $ consumeClientMessage ptr
        waitKM acceptsRepeats

exitX :: X ()
exitX = mzero

pointerPos :: X (Position, Position)
pointerPos = do
    XEnv { xdisplay = dpy, xroot = root, xlastevent = ptr} <- ask
    liftIO $ do
        typ <- if ptr == nullPtr then return 0 else get_EventType ptr
        if typ > 2 then do
            ev <- getEvent ptr
            return (fromIntegral $ ev_x_root ev, fromIntegral $ ev_y_root ev)
        else do
            (_, _, _, x, y, _, _, _) <- queryPointer dpy root
            return (fromIntegral x,fromIntegral y)

relPointerPos :: Window -> X (Position, Position)
relPointerPos w = do
    XEnv { xdisplay = dpy } <- ask
    liftIO $ do
        (_,_,_,_,_, x, y, _) <- queryPointer dpy w
        return (fromIntegral x, fromIntegral y)

pointerProp :: X (Double, Double)
pointerProp = do
    XEnv { xdisplay = dpy } <- ask
    target <- currentWindow
    (x, y) <- relPointerPos target
    (_,_,_, w, h, _,_) <- liftIO $ getGeometry dpy target
    return (fromIntegral x/fromIntegral w, fromIntegral y/fromIntegral h)

currentWindow :: X Window
currentWindow = do
    XEnv { xdisplay = dpy } <- ask
    (w, _) <- liftIO $ getInputFocus dpy
    return w

setPointerPos :: Position -> Position -> X ()
setPointerPos x y = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    liftIO $ warpPointer dpy 0 root 0 0 0 0 x y
    return ()

inCurrentPos :: X a -> X a
inCurrentPos f = do
    XEnv { xdisplay = dpy } <- ask
    (x,y) <- pointerPos 
    w <- currentWindow
    ret <- f
    setPointerPos x y
    liftIO $ setInputFocus dpy w 0 0
    return ret 

modifyBindings :: (Bindings -> Bindings) -> X ()
modifyBindings f = do
    s@XControl { xbindings = b } <- get
    setBindings (f b)

eventToKM' :: XEventPtr -> IO (Time, KM)
eventToKM' ptr = do
    typ <- get_EventType ptr
    let up = any (== typ) [keyRelease, buttonRelease]
    if any (== typ) [keyPress, keyRelease ] then do
        (_,_,t,_,_,_,_, st, kc, _) <- get_KeyEvent ptr
        return (t, KM up (0x1fff .&. st) (KCode kc))
    else if any (== typ) [buttonPress, buttonRelease] then do
        (_,_,t,_,_,_,_, st, mb, _) <- get_ButtonEvent ptr
        return (t, KM up (0x1fff .&. st) (MButton mb))
    else
        return $ (0, nullKM)

eventToKM :: XEventPtr -> IO KM
eventToKM ptr = snd `liftM` eventToKM' ptr

askKM :: X KM
askKM = ask >>= io . eventToKM . xlastevent    

askKeysym :: X (Maybe KeySym, String)
askKeysym = do
    XEnv { xlastevent = ev } <- ask
    let kev = asKeyEvent ev
    io $ lookupString kev

bind :: [KM] -> X () -> X ()
bind [] _ = return ()
bind kms act = do
    kms' <- traverse normalizeKM kms
    modifyBindings (insert kms' act)

unbind :: [KM] -> X ()
unbind = modifyBindings . delete

printBindings :: X ()
printBindings =
    get >>= liftIO . putStrLn . drawNMap . xbindings 

windowPid :: Window -> X (Maybe CPid)
windowPid win = do
    XEnv { xdisplay = dpy } <- ask
    liftIO $ do
        atom <- internAtom dpy "_NET_WM_PID" False
        mpid <- getWindowProperty32 dpy atom win
        return $ fmap (fromIntegral . head) mpid


