module XHotkey.Core where

import XHotkey.Types
import Data.MapTree

import Graphics.X11
import Graphics.X11.Xlib.Extras
import System.Posix.Process
import System.Posix.Types

import Data.Word

import Control.Monad.State
import Control.Monad.Reader

import Control.Concurrent
import Foreign
import qualified Data.List as L
import qualified Data.Map as M
import Prelude hiding (lookup)

runX :: (X a) -> XEnv -> XControl -> IO (a, XControl)
runX (X a) env control = runStateT (runReaderT a env) control

runX' :: (X a) -> IO a
runX' m = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    (ret, st) <- allocaXEvent $ \e -> fillBytes e 0 196 >> runX m (XEnv dpy root e 0) (XControl mempty False)
    closeDisplay dpy
    return ret

mainLoop :: X ()
mainLoop = do
    s@XControl { hkMap = hk } <- get
    hk2 <- traverseKeys normalizeKM hk
    put $ s { hkMap = hk2 }
    loop mempty
    return ()
    where 
      loop :: [KM] -> X ()
      loop hk = do
        XControl { hkMap = hk', exitScheduled = ext } <- get
        if ext then
            return ()
        else do
            mapM_ _grabKM (baseKeys hk' L.\\ hk)
            mapM_ _ungrabKM (hk L.\\ (baseKeys hk'))
            XEnv { display = dpy, rootWindow' = root, currentEvent = ptr } <- ask
            liftIO $ nextEvent dpy ptr
            km <- liftIO $ ptrToKM ptr
            case do
                km' <- km
                x <- lookup km' hk'
                case x of
                    Left m -> return $ do
                        liftIO $ do 
                            grabKeyboard dpy root False grabModeAsync grabModeAsync currentTime
                            maskEvent dpy (buttonPressMask .|. buttonReleaseMask .|. keyPressMask .|. keyReleaseMask) ptr
                        grabbedLoop m
                        liftIO $ ungrabKeyboard dpy currentTime
                        return ()
                    Right x -> return x
                of
                Just x -> x
                Nothing -> return ()
            loop (baseKeys hk')
                
      ptrToKM :: XEventPtr -> IO (Maybe KM)
      ptrToKM ptr = do
        evt <- get_EventType ptr
        let up = any (== evt) [keyRelease, buttonRelease]
        if any (== evt) [keyPress, keyRelease ] then do
            (_,_,_,_,_,_,_, st, kc, _) <- get_KeyEvent ptr
            return $ Just $ KM up (0x1fff .&. st) (KCode kc)
        else if any (== evt) [buttonPress, buttonRelease] then do
            (_,_,_,_,_,_,_, st, mb, _) <- get_ButtonEvent ptr
            return $ Just $ KM up (0x1fff .&. st) (MButton mb)
        else
            return Nothing
      grabbedLoop :: Bindings -> X ()
      grabbedLoop m = do
        XEnv { display = dpy, rootWindow' = root, currentEvent = ptr } <- ask
        liftIO $ nextEvent dpy ptr
        km <- liftIO $ ptrToKM ptr
        case do
            km' <- km
            x <- lookup km' m
            case x of
                Left m' -> return $ grabbedLoop m'
                Right x -> return x
            of
            Just x -> x
            Nothing -> return ()
        
            
    
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
    xenv <- ask
    xctrl <- get
    io $ forkIO $ runX x xenv xctrl >> return ()

forkP :: MonadIO m => IO () -> m ProcessID
forkP prog = io . forkProcess $ do
    createSession
    prog

spawnPID :: MonadIO m => String -> m ProcessID
spawnPID prog = forkP $ executeFile "/bin/sh" False ["-c", prog] Nothing

spawn :: MonadIO m => String -> m ()
spawn prog = spawnPID prog >> return ()

flushX :: X ()
flushX = do
    XEnv { display = dpy } <- ask
    liftIO $flush dpy
    
exitX :: X()
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

setBindings :: Bindings -> X ()
setBindings b = do
    xc <- get
    put $ xc { hkMap = b }
    
hotkey :: [KM] -> X () -> X ()
hotkey kms act = do
    xc@XControl { hkMap = hk, exitScheduled = ext } <- get
    put xc { hkMap = insert kms act hk }

printBindings :: X ()
printBindings =
    get >>= liftIO . putStrLn . drawMapTree . hkMap

