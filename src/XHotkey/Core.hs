{-# LANGUAGE DoAndIfThenElse #-}

module XHotkey.Core where

import XHotkey.Types
import Data.NMap

import Graphics.X11
import Graphics.X11.Xlib.Extras
import System.Posix.Process
import System.Posix.Types

import Data.Word

import Control.Monad.State
import Control.Monad.Reader

import Control.Concurrent
import Foreign
import Foreign.C
import qualified Data.List as L
import qualified Data.Map as M
import Prelude hiding (lookup)

runX :: (X a) -> XEnv -> XControl -> IO (a, XControl)
runX (X a) env control = runStateT (runReaderT a env) control

runX' :: (X a) -> IO a
runX' m = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    (ret, st) <- allocaXEvent $ 
        \e -> fillBytes e 0 196 >> runX m (XEnv dpy root e) (XControl mempty False)
    closeDisplay dpy
    return ret

mainLoop :: X ()
mainLoop = do
    s@XControl { hkMap = hk } <- get
    hk2 <- traverseKeys normalizeKM hk
    put $ s { hkMap = hk2 }
    tmp <- io $ callocBytes 196 :: X XEventPtr
    loop tmp mempty
    io $ free tmp
    return ()
    where 
      loop :: XEventPtr -> [KM] -> X ()
      loop tmp hk = do
        XControl { hkMap = hk', exitScheduled = ext } <- get
        if ext then
            return ()
        else do
            mapM_ _grabKM (rootKeys hk' L.\\ hk)
            mapM_ _ungrabKM (hk L.\\ (rootKeys hk'))
            XEnv { display = dpy, rootWindow' = root, currentEvent = ptr } <- ask
            liftIO $ nextEvent dpy ptr
            nevs <- io $ eventsQueued dpy queuedAfterReading
            km <- ptrToKM tmp nevs
            -- io $ print km
            case do
                km' <- km
                x <- lookup km' hk'
                case x of
                    Left m -> return $ do
                        liftIO $ do 
                            grabKeyboard dpy root False grabModeAsync grabModeAsync currentTime
                            grabPointer dpy root False (buttonPressMask .|. buttonReleaseMask) grabModeAsync grabModeAsync 0 0 0 
                            maskEvent dpy (buttonPressMask .|. buttonReleaseMask .|. keyPressMask .|. keyReleaseMask) ptr
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
            loop tmp (rootKeys hk')
                
      ptrToKM :: XEventPtr -> CInt -> X (Maybe KM)
      ptrToKM ptr n = do
        XEnv { display = dpy, currentEvent = cur } <- ask
        (t0, km) <- io $ eventToKM' cur
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
            nextEvent dpy ptr
            return Nothing
        else
            return (Just km)
      grabbedLoop :: XEventPtr -> Bindings -> X (X ())
      grabbedLoop tmp m = do
        XEnv { display = dpy, rootWindow' = root, currentEvent = ptr } <- ask
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
    XEnv { display = dpy, rootWindow' = root } <- ask
    (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ grabKey dpy c st root False grabModeAsync grabModeAsync
        MButton b -> liftIO $ grabButton dpy b st root False (buttonPressMask .|. buttonReleaseMask) grabModeAsync grabModeAsync root 0

_ungrabKM :: KM -> X ()
_ungrabKM k = do
    XEnv { display = dpy, rootWindow' = root } <- ask
    (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ ungrabKey dpy c st root
        MButton b -> liftIO $ ungrabButton dpy b st root
    return ()

io :: MonadIO m => IO a -> m a
io = liftIO


forkX :: X () -> X ThreadId
forkX x = do
    xenv@XEnv { currentEvent = ptr } <- ask
    xctrl <- get
    io $ forkIO $ allocaXEvent $ \ptr' -> do
        copyBytes ptr' ptr 196 
        runX x xenv { currentEvent = ptr' } xctrl >> return ()

forkX_ :: X () -> X ()
forkX_ = (>> return ()) . forkX

forkP :: MonadIO m => IO () -> m ProcessID
forkP prog = io . forkProcess $ do
    createSession
    prog

forkP_ :: MonadIO m => IO () -> m ()
forkP_ = (>> return ()) . forkP

spawnPID :: MonadIO m => String -> m ProcessID
spawnPID prog = forkP $ executeFile "/bin/sh" False ["-c", prog] Nothing

spawn :: MonadIO m => String -> m ()
spawn prog = spawnPID prog >> return ()

currentEventType :: X EventType
currentEventType = ask >>= liftIO . get_EventType . currentEvent

flushX :: X ()
flushX = do
    XEnv { display = dpy } <- ask
    liftIO $flush dpy
    
exitX :: X ()
exitX = do
    s <- get
    put $ s {exitScheduled = True}

pointerPos :: X (Position, Position)
pointerPos = do
    XEnv { display = dpy, rootWindow' = root, currentEvent = ptr} <- ask
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
    XEnv { display = dpy } <- ask
    liftIO $ do
        (_,_,_,_,_, x, y, _) <- queryPointer dpy w
        return (fromIntegral x, fromIntegral y)

pointerProp :: X (Double, Double)
pointerProp = do
    XEnv { display = dpy } <- ask
    target <- currentWindow
    (x, y) <- relPointerPos target
    (_,_,_, w, h, _,_) <- liftIO $ getGeometry dpy target
    return (fromIntegral x/fromIntegral w, fromIntegral y/fromIntegral h)

currentWindow :: X Window
currentWindow = do
    XEnv { display = dpy } <- ask
    (w, _) <- liftIO $ getInputFocus dpy
    return w

setPointerPos :: Position -> Position -> X ()
setPointerPos x y = do
    XEnv { display = dpy, rootWindow' = root } <- ask
    liftIO $ warpPointer dpy 0 root 0 0 0 0 x y
    return ()

inCurrentPos :: X a -> X a
inCurrentPos f = do
    XEnv { display = dpy } <- ask
    (x,y) <- pointerPos 
    w <- currentWindow
    ret <- f
    setPointerPos x y
    liftIO $ setInputFocus dpy w 0 0
    return ret 

modifyBindings :: (Bindings -> Bindings) -> X ()
modifyBindings f = do
    s@XControl { hkMap = b } <- get
    put $ s { hkMap = f b }

setBindings :: Bindings -> X ()
setBindings b = do
    b' <- mapKeysM normalizeKM b
    modify $ \s -> s { hkMap = b' }

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
askKM = ask >>= io . eventToKM . currentEvent    

askKeysym :: X (Maybe KeySym, String)
askKeysym = do
    XEnv { currentEvent = ev } <- ask
    let kev = asKeyEvent ev
    io $ lookupString kev

bind :: [KM] -> X () -> X ()
bind kms act = do
    xc@XControl { hkMap = hk, exitScheduled = ext } <- get
    put xc { hkMap = insert kms act hk }

unbind :: [KM] -> X ()
unbind = modifyBindings . delete

printBindings :: X ()
printBindings =
    get >>= liftIO . putStrLn . drawNMap . hkMap 
