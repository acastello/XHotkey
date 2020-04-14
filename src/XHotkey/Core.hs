{-# LANGUAGE DoAndIfThenElse, OverloadedStrings, BangPatterns, CPP #-}

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
import qualified Data.Set as S
import qualified Data.Tree as T

import Foreign hiding (void)
import Foreign.C

import Prelude hiding (lookup)

import System.Process (callCommand)

import Text.Printf (printf)


withX :: X a -> XEnv -> XControl -> IO (a, XControl)
withX (X a) env control = runStateT (runReaderT a env) control

runX :: (X a) -> IO a
runX m = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    ret <- allocaXEvent $ \e -> allocaXEvent $ \e' -> try $
        fillBytes e 0 xeventsize >> withX m (XEnv dpy root e e')
        (defaultXControl dpy)
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

setAutorepeat :: Bool -> X ()
setAutorepeat whether = modify $ \xctrl -> xctrl { xrepeatkeys = whether }

waitX :: X ()
waitX = do
    return ()

setBindings :: Bindings -> X ()
setBindings binds = do
    XControl { xtargets = tars } <- get
    setBindingsTargets binds tars

setTargets :: [Window] -> X ()
setTargets tars = do
    XControl { xbindings = binds } <- get
    setBindingsTargets binds tars

setBindingsTargets :: Bindings -> [Window] -> X ()
setBindingsTargets rawbinds tars = do
    xctrl @ XControl { xbindings = oldbinds, xtargets = oldtars } <- get
    binds <- traverseKeys normalizeKM rawbinds
    let oldbase = rootKeys oldbinds
        newbase = rootKeys binds
    mapM2 _ungrabKM oldbase (oldtars L.\\ tars)
    mapM2 _ungrabKM (oldbase L.\\ newbase) (tars L.\\ oldtars)
    mapM2 _grabKM newbase (tars L.\\ oldtars)
    mapM2 _grabKM (newbase L.\\ oldbase) tars

    put xctrl { xbindings = binds, xtargets = tars }

    where
        mapM2 f xs ys = sequence_ [f x y | x <- xs, y <- ys]

-- TODO: Change name
mainLoop' :: X ()
mainLoop' = baseLoop' S.empty False

mainLoop :: X ()
mainLoop = void $ runStateT baseLoop
              $ GrabState False False mempty mempty mempty

baseLoop' :: S.Set KeyCode -> Bool -> X ()
baseLoop' oldp grabbed = do
    XControl { xbindings = binds } <- get
    km <- waitKM
    let newp = procSet km

    grabbed' <- if not grabbed && S.size newp > 1 then do
            _grabKeyboard
            return True
        else
            return grabbed

    newerp <- case lookup km binds of
        Nothing -> do
            when (not $ member km { keyUp = not $ keyUp km } binds) $ do
                fallThrough
            return newp
        Just (Right op) -> do
            op
            return newp
        Just (Left map) -> do
            if not grabbed' then do
                _grabKeyboard
                ret <- grabbedLoop' newp map
                when (S.size ret == 0)
                    _ungrabKeyboard
                return ret
            else
                grabbedLoop' newp map

    grabbed'' <- if grabbed' && S.size newerp == 0 then do
            _ungrabKeyboard
            return False
        else
            return grabbed'

    baseLoop' newerp grabbed''

    where
        procSet :: KM -> S.Set KeyCode
        procSet km = maybe oldp (\k ->
            if keyUp km then
                S.delete k oldp
            else
                S.insert k oldp) (kisKeyCode km)

baseLoop :: GrabEnv ()
baseLoop = do
    checkgrabs
    km <- lift waitKM
    procset km
    XControl { xbindings = map } <- lift get
    GrabState { gKeys = lastmap } <- get
    case lookup km map of
        Nothing -> do
            when (not $ member km { keyUp = not $ keyUp km } map) $ do
                lift fallThrough
        Just (Right op) -> do
            lift op
        Just (Left map') -> do
            st @ GrabState { gKeyboard = grabbed } <- get
            unless grabbed $ do
                lift _grabKeyboard
                put st { gKeyboard = True }
            modify $ \s -> s { gMap = map' }
            grabbedLoop km
    baseLoop
    where
        procset :: KM -> GrabEnv ()
        procset km = do
            st @ GrabState { gKeys = keys, gButtons = buttons } <- get
            case km of
                KM { mainKey = KCode kc } -> do
                    let f = if keyUp km then S.delete else S.insert
                    put st { gKeys = f kc keys }
                KM { mainKey = MButton bt } -> do
                    let f = if keyUp km then S.delete else S.insert
                    put st { gButtons = f bt buttons }
                _ -> return ()
        checkgrabs :: GrabEnv ()
        checkgrabs = do
            st @ GrabState { gKeys = keys, gKeyboard = keyboard } <- get
            when (not keyboard && not (S.null keys)) $ do
                lift _grabKeyboard
                put st { gKeyboard = True }
            when (keyboard && S.null keys) $ do
                lift _ungrabKeyboard
                put st { gKeyboard = False }
        resetmap :: GrabEnv ()
        resetmap = do
            XControl { xbindings = map } <- lift get
            modify $ \s -> s { gMap = map }

-- called recursively
grabbedLoop' :: S.Set KeyCode -> Bindings -> X (S.Set KeyCode)
grabbedLoop' pressed map = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    km <- waitKM
    -- io $ print km
    let pressed' = procSet pressed km
    case lookup km map of
        Nothing -> do
            return pressed'
        Just (Right op) -> do
            op
            return pressed'
        Just (Left map') -> do
            grabbedLoop' pressed' map'

    where
        procSet :: S.Set KeyCode -> KM -> S.Set KeyCode
        procSet set km = maybe set (\k ->
            if keyUp km then
                S.delete k pressed
            else
                S.insert k pressed) (kisKeyCode km)

grabbedLoop :: KM -> GrabEnv ()
grabbedLoop orig = do
    GrabState { gMap = map } <- get
    km <- lift waitKM
    procset km
    case lookup km map of
        Nothing -> do
            when (member km { keyUp = not $ keyUp km } map) $ do
                redo
        Just (Right op) -> do
            lift op
            redo
        Just (Left map') -> do
            modify $ \s -> s { gMap = map' }
            grabbedLoop km
            redo
    where
        redo :: GrabEnv ()
        redo = when (not $ keyUp orig) $ do
            GrabState { gKeys = keys, gButtons = buttons } <- get
            flip when (grabbedLoop orig) $ case orig of
                KM { mainKey = KCode kc } -> kc `S.member` keys
                KM { mainKey = MButton bt } -> bt `S.member` buttons
                _ -> False
        procset :: KM -> GrabEnv ()
        procset km = do
            st @ GrabState { gKeys = keys, gButtons = buttons } <- get
            case km of
                KM { mainKey = KCode kc } -> do
                    let f = if keyUp km then S.delete else S.insert
                    put st { gKeys = f kc keys }
                KM { mainKey = MButton bt } -> do
                    let f = if keyUp km then S.delete else S.insert
                    put st { gButtons = f bt buttons }
                _ -> return ()

{- mainLoop' :: X ()
mainLoop' = do
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
                    x <- grabbedLoop' tmp m
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
      grabbedLoop' :: XEventPtr -> Bindings -> X (X ())
      grabbedLoop' tmp m = do
        XEnv { xdisplay = dpy, xroot = root, xlastevent = ptr } <- ask
        liftIO $ nextEvent dpy ptr
        nevs <- io $ eventsQueued dpy queuedAfterReading
        km <- ptrToKM tmp nevs
        case do
            km' <- km
            x <- lookup km' m
            case x of
                Left m' -> return $ grabbedLoop' tmp m'
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
        -}

_grabKeyboard :: X ()
_grabKeyboard = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    void $ io $ grabKeyboard dpy root True grabModeAsync grabModeAsync 0

_ungrabKeyboard :: X ()
_ungrabKeyboard = do
    XEnv { xdisplay = dpy } <- ask
    void $ io $ ungrabKeyboard dpy 0

_grabKM :: KM -> Window -> X ()
_grabKM k win = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    k @ (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ grabKey dpy c st win False grabModeAsync
                                grabModeAsync
        MButton b -> liftIO $ grabButton dpy b st win False
                                (buttonPressMask .|. buttonReleaseMask)
                                grabModeAsync grabModeAsync root 0

_ungrabKM :: KM -> Window -> X ()
_ungrabKM k win = do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ ungrabKey dpy c st win
        MButton b -> liftIO $ ungrabButton dpy b st win
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

windowTree :: Window -> X ([Window], T.Tree Window)
windowTree win = do
    XEnv { xroot = root, xdisplay = dpy } <- ask
    (_,p,c) <- io (queryTree dpy win)
    liftM2 (,) (parents dpy root p) (T.Node win <$> mapChildren return `mapM` c)
    where
    parents dpy root w
      | root == w = return [w]
      | otherwise = do
          (_,p,_) <- io (queryTree dpy w)
          (++ [w]) <$> parents dpy root p

windowsTree :: X (T.Tree Window)
windowsTree = forWindows return

mapChildren :: (Window -> X a) -> Window -> X (T.Tree a)
mapChildren f win = do
    XEnv { xdisplay = dpy } <- ask
    children <- (\(_,_,z) -> z) <$> io (queryTree dpy win)
    liftM2 T.Node (f win) $ mapM (mapChildren f) children

forWindows :: (Window -> X a) -> X (T.Tree a)
forWindows f = do
    XEnv { xroot = root } <- ask
    mapChildren f root

foldWindows :: (b -> Window -> X b) -> b -> X b
foldWindows op e = do
    XEnv { xroot = root, xdisplay = dpy } <- ask
    forWinA dpy e root where
    forWinA dpy e win = do
        ret <- op e win
        children <- io $ return . (\(_,_,y) -> y) =<< queryTree dpy win
        liftIO $ forM_ children (addToSaveSet dpy)
        ret <- foldM (forWinA dpy) ret children
        liftIO $ removeFromSaveSet dpy win
        return ret

fallThrough :: X ()
fallThrough = do
    XEnv { xdisplay = dpy, xlastevent = lastev } <- ask
    io $ do
        (win, _) <- getInputFocus dpy
        setEventWindow win lastev
        sendEvent dpy win False keyPressMask lastev
    flushX

-- | Wait for a KeyPress, KeyRelease, ButtonPress or ButtonRelease, execute any
-- ClientMessage if necessary, will throw an exception if
waitKM :: X KM
waitKM = do
    XEnv { xdisplay = dpy
         , xroot = root
         , xlastevent = ptr
         , xtempevent = tmp } <- ask
    XControl { xrepeatkeys = acceptsRepeats } <- get
    liftIO $ nextEvent dpy ptr
    typ <- io $ get_EventType ptr
    if any (typ ==) [keyPress, keyRelease, buttonPress, buttonRelease] then do
        (t,km) <- io $ eventToKM' ptr
#ifdef DEBUG
        io $ printf "[waitKM] received %s\n" (show km)
#endif
        if acceptsRepeats then
            return km
        else do
            nevs <- io $ eventsQueued dpy queuedAfterReading
            if nevs > 0 then do
                (t', km') <- io $ do
                    peekEvent dpy tmp
                    eventToKM' tmp
                if t' - t < 40 && km' == km { keyUp = not $ keyUp km } then do
                    io $ nextEvent dpy tmp
                    waitKM
                else do
                    io $ copyBytes ptr tmp xeventsize
                    return km
            else
                return km
    else do
        when (typ == clientMessage) $
            join $ io $ consumeClientMessage ptr
        waitKM

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

printTargets :: X ()
printTargets = do
    XEnv { xdisplay = dpy } <- ask
    XControl { xtargets = targets } <- get
    strs <- liftIO $ forM targets $ \win -> do
        ch <- getClassHint dpy win
        return $ printf "%#x %s %s" win (show $ resName ch) (show $ resClass ch)
    liftIO $ putStrLn $ concat $ ["[", concat $ L.intersperse ", " strs, "]"]

windowPid :: Window -> X (Maybe CPid)
windowPid win = do
    XEnv { xdisplay = dpy } <- ask
    liftIO $ do
        atom <- internAtom dpy "_NET_WM_PID" False
        mpid <- getWindowProperty32 dpy atom win
        return $ fmap (fromIntegral . head) mpid

test :: IO ()
test = runX $ do
    bind ["q"] exitX
    bind ["w"] printBindings
    bind ["1"] (io $ putStrLn "1 down")
    bind ["1 up"] (io $ putStrLn "1 up")
    bind ["2"] (io $ putStrLn "2 down")
    bind ["2 up"] (io $ putStrLn "2 up")
    bind ["3", "1"] (io $ putStrLn "3, 1")
    bind ["ctrl-w"] (currentWindow >>= setTargets . pure)
    bind ["ctrl-r"] (ask >>= setTargets . pure . xroot)
    mainLoop
