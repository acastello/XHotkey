{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module XHotkey.Types 
    where

import Data.Alternative

import qualified Data.NMap as M
import Data.NMap hiding (lookup)

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State

import Graphics.X11
import Graphics.X11.Xlib.Extras (Event)

import Data.Word
import Data.Bits
import Data.Char (toLower)
import Data.IORef
import Data.Maybe (fromMaybe)

import Data.String

import Foreign
import Foreign.C

import System.Posix
import System.Posix.IO

import qualified Text.Read as T
import Text.Read.Lex (numberToInteger)
import Numeric (showHex)

infixl 7 .<. 
word .<. shift = shiftL word shift

infixl 7 .>. 
word .>. shift = shiftR word shift

#include <X11/Xlib.h>

deriving instance Storable (Display)

callocXEvent :: IO (XEventPtr)
callocXEvent = callocBytes #{size XEvent}

data MChan m a = MChan_ (MVar (Either () (m a))) (MVar a) (IORef Bool)

newMChan :: MonadIO m' => m' (MChan m a)
newMChan = io $ do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    running <- newIORef True
    return (MChan_ mv1 mv2 running)

closeMChan :: MonadIO m' => MChan m a -> m' ()
closeMChan (MChan_ v1 _ st) = io $ do
    running <- readIORef st
    if running then do
        putMVar v1 (Left ())
        writeIORef st False
    else do
        tryPutMVar v1 (Left ()) 
        return ()

evalMChan :: MonadIO m' => MChan m a -> m a -> m' a
evalMChan (MChan_ v1 v2 st) action = io $ do
    running <- readIORef st
    if running then do
        putMVar v1 (Right action)
        takeMVar v2 
    else
        error "MChan is closed."

evalMChan_ :: (Monad m, MonadIO m') => MChan m a -> m () -> m' a
evalMChan_ chan act = evalMChan chan (act >> return undefined)

runMChan :: MonadIO m' => MChan m a -> (m a -> m' a) -> m' Bool
runMChan (MChan_ mv1 mv2 st) handle = do
    running <- io $ readIORef st
    if running then do
        par <- io $ takeMVar mv1
        case par of
            Left _ -> return False
            Right par' -> do
                result <- handle par'
                io $ putMVar mv2 result
                return True
    else
        return False

tryEvalMChan (MChan_ v1 v2 st) action = io $ do
    running <- readIORef st
    if running then do
        succ <- tryPutMVar v1 (Right action)
        if succ then 
            Just <$> takeMVar v2
        else
            return Nothing
    else
        error "MChan is closed."

tryRunMChan :: MonadIO m' => MChan m a -> (m a -> m' a) -> m' Bool
tryRunMChan (MChan_ mv1 mv2 st) handle = do
    running <- io $ readIORef st
    if running then do
        par <- io $ tryTakeMVar mv1
        case par of
            Nothing -> return True
            Just (Left _) -> return False
            Just (Right par') -> do
                result <- handle par'
                io $ putMVar mv2 result
                return True
    else
        return False

foreverChan :: MonadIO m' => MChan m a -> (m a -> m' a) -> m' ()
foreverChan chan act = do
    rep <- runMChan chan act
    when rep $ foreverChan chan act


-- | XEnv
data XEnv = XEnv
    { display   :: Display
    , rootWindow'   :: !Window
    , currentEvent :: XEventPtr
    } deriving Show

data XControl = XControl
    { hkMap :: Bindings
    , exitScheduled :: Bool
    } deriving Show

defaultXControl :: XControl
defaultXControl = XControl mempty False

data Hook 
    = OnClear (X ())
    | OnKM (KM -> X ())
    | OnExit (X ())
    | OnGrab (KM -> Bindings -> X ())
    | OnAction { onAction :: KM -> X () -> X () }
    | OnLoop (X ())

type ForkedX = (XEnv, Window, ThreadId)

type Bindings = M.NMap KM (X ())

drawBindings :: Bindings -> String
drawBindings = drawNMap

-- | X reader monad
newtype X a = X (ReaderT XEnv (StateT XControl IO) a)
    deriving (Functor, Applicative, Monad, MonadReader XEnv, MonadState XControl
             , MonadIO, Alternative, MonadPlus)

instance Show (X a) where
    show _ = "X ()"    

type XChan = MChan X

newXChan :: MonadIO m => m (XChan a)
newXChan = newMChan

closeXChan :: MonadIO m => XChan a -> m ()
closeXChan xc = do
    evalMChan_ xc (modify $ \s -> s { exitScheduled = True })
    closeMChan xc

-- | Key and Mouse wrapper
data KM = KM 
    { keyUp :: Bool
    , keyModifiers :: Modifier
    , mainKey :: KMitem }
    deriving (Eq)

nullKM = KM False 0 (KCode 0 )

data KMitem = 
        KCode KeyCode
      | KSym KeySym
      | MButton Button
    deriving (Eq, Ord)

instance Show KMitem where
    show (KCode c) = "0x" ++ showHex c ""
    show (KSym s) = case keysymToString s of
        [] -> "(keysym: " ++ show s ++ ", no symbol)"
        s' -> s'
    show (MButton b) = "mouse" ++ (show b)

instance Enum KMitem where
    fromEnum (KCode c) = (0 `shiftL` 25) .|. (0x1ffffff .&. (fromIntegral c))
    fromEnum (KSym s) =  (1 `shiftL` 25) .|. (0x1ffffff .&. (fromIntegral s))
    fromEnum (MButton b) = (2 `shiftL` 25) .|. (0x1ffffff .&. (fromIntegral b))
    toEnum n
        | n' == 0   = KCode   $ fromIntegral n''
        | n' == 1   = KSym $ fromIntegral n''
        | otherwise = MButton $ fromIntegral n''
        where n' = n `shiftR` 25 .&. 0x3
              n'' = n .&. 0x1ffffff

instance Read KMitem where
        readPrec = T.parens $ do 
                T.choice 
                    [ do
                            str <- T.choice
                               [do 
                                    T.Ident str' <- T.lexP
                                    return str'
                               ,do
                                    n' <- T.readPrec :: T.ReadPrec Integer
                                    return (show n')]
                            let ks = stringToKeysym str
                            if ks == 0 then fail "invalid keysym string" else return (KSym ks)
                    , do
                            lit "c"
                            c <- T.get
                            return (KSym $ fromIntegral $ fromEnum c)
                    , do
                            lit "k"
                            n <- T.readPrec
                            return (KCode n)
                    , do
                            lit "m" T.+++ lit "mouse"
                            n <- T.readPrec
                            return (MButton n) ]

instance Show KM where
    show (KM up mod k) = 
        let s = foldMap state' (listModifiers mod)
        in s ++ (show k) ++ (if up then " Up" else "")
        where state' kc = fromMaybe "" $ lookup kc
                [ (shiftMask, "Shift-"), (lockMask, "Caps-"), (controlMask, "Ctrl-")
                , (mod1Mask, "Mod1-"), (mod2Mask, "Mod2-"), (mod3Mask, "Mod3-")
                , (mod4Mask, "Mod4-"), (mod5Mask, "Mod5-"), (button1Mask, "Btn1-")
                , (button2Mask, "Btn2-"), (button3Mask, "Btn3-")
                , (button4Mask, "Btn4-"), (button5Mask, "Btn5-") ]

instance Read KM where
    readPrec = T.parens $ do 
        s <- readState 0
        kc <- T.readPrec
        onrel <- T.choice
            [ do
                '\'' <- T.get
                return True
            , do
                lit " up"
                return True
            , do
                return False ]
        return $ KM onrel s kc
        where
            readState st = T.choice
                ((\(k,s) -> do
                    lit k
                    readState (st .|. s))
                <$> modMap)
                T.+++ do
                    return st
            modMap = foldMap (uncurry zip) $ fmap repeat <$>
                [ (["S-", "Shift-", "shift "], shiftMask)
                , (["Caps-", "CapsLock "], lockMask)
                , (["C-", "Ctrl-", "ctrl ", "control "], controlMask)
                , (["mod1-", "mod1 ", "M-", "meta ", "A-", "Alt-", "alt "], mod1Mask)
                , (["mod2-", "mod2 ", "Num-", "NumLock "], mod2Mask)
                , (["mod3-", "mod3 ", "Scroll-", "ScrollLock "], mod3Mask)
                , (["mod4-", "mod4 ", "Win-", "Win ", "Cmd-", "Cmd ", "Super "], mod4Mask)
                , (["mod5-", "mod5 "], mod5Mask)
                , (["btn1-", "button1 "], button1Mask)
                , (["btn2-", "button2 "], button2Mask)
                , (["btn3-", "button3 "], button3Mask)
                , (["btn4-", "button4 "], button4Mask)
                , (["btn5-", "button5 "], button5Mask) ]

instance Ord KM where
    KM u1 m1 k1 `compare` KM u2 m2 k2 = compare k1 k2 `mappend` compare m1 m2 `mappend` compare u1 u2

instance Enum KM where
    fromEnum (KM up st k) = ((fromEnum up) `shiftL` 59) .|. ((fromIntegral st) `shiftL` 27) .|. fromEnum k
    toEnum n = KM (testBit n 59) (fromIntegral $ n `shiftR` 27 .&. 0xfff) (toEnum $ n .&. 0x7ffffff)

instance IsString KM where
    fromString = read

fromChar :: Char -> KM
fromChar = KM False 0 . KSym . fromIntegral . fromEnum

up_ :: KM -> KM
up_ (KM _ st k) = KM True st k

addModifier :: Modifier -> KM -> KM
addModifier m k = k { keyModifiers = (keyModifiers k) .|. m }

shift_ :: KM -> KM
shift_ = addModifier shiftMask

caps_ :: KM -> KM
caps_ = addModifier lockMask

ctrl_ :: KM -> KM
ctrl_ = addModifier controlMask

mod1_ :: KM -> KM
mod1_ = addModifier mod1Mask 
alt_ = mod1_

mod2_ :: KM -> KM
mod2_ = addModifier mod2Mask
num_ = mod2_

mod3_ :: KM -> KM
mod3_ = addModifier mod3Mask
scroll_ = mod3_

mod4_ :: KM -> KM
mod4_ = addModifier mod4Mask
win_ = mod4_

mod5_ :: KM -> KM
mod5_ = addModifier mod5Mask

modifiersMask :: Modifier
modifiersMask = sum [1 `shiftL` i | i <- [0..12]]

listModifiers :: Modifier -> [Modifier]
listModifiers s = [s .&. (1 `shiftL` i) | i <- [0..12], testBit s i]

normalizeKM :: KM -> X KM
normalizeKM (KM u s (KSym ks)) = do
    XEnv {display = dpy} <- ask
    kc <- liftIO $ keysymToKeycode dpy ks
    return (KM u s (KCode kc))
normalizeKM km = return km

symfyKM :: KM -> X KM
symfyKM (KM u s (KCode kc)) = do
    XEnv { display = dpy } <- ask
    ks <- liftIO $ keycodeToKeysym dpy kc 0
    return (KM u s (KSym ks))
symfyKM km = return km


type ClientMessage = XEventPtr

newClientMessage :: Display -> Window -> X () ->  IO ClientMessage
newClientMessage display win act = do
    sptr <- newStablePtr act
    ptr <- callocBytes #size XEvent
    (#poke XClientMessageEvent, type) ptr clientMessage
    (#poke XClientMessageEvent, send_event) ptr (0 :: CInt)
    (#poke XClientMessageEvent, display) ptr display
    (#poke XClientMessageEvent, window) ptr win
    (#poke XClientMessageEvent, format) ptr (32 :: CInt)
    (#poke XClientMessageEvent, data) ptr (castStablePtrToPtr sptr)
    return ptr

freeClientMessage :: ClientMessage -> IO ()
freeClientMessage msg = do
    ptr <- (#peek XClientMessageEvent, data) msg
    freeStablePtr $ castPtrToStablePtr ptr
    free msg

peekClientMessage :: ClientMessage -> IO (X ())
peekClientMessage msg = do
    ptr <- (#peek XClientMessageEvent, data) msg
    deRefStablePtr ptr

consumeClientMessage :: ClientMessage -> IO (X ())
consumeClientMessage msg = do
    ptr <- castPtrToStablePtr <$> (#peek XClientMessageEvent, data) msg
    act <- deRefStablePtr ptr
    freeStablePtr ptr
    return act

sendClientMessage :: Display -> ClientMessage -> IO ()
sendClientMessage dpy msg = do
    let fd = Fd $ connectionNumber dpy
    fdWriteBuf fd (castPtr msg) (#size XEvent)
    return ()


--
-- utils
--

io :: MonadIO m => IO a -> m a
io = liftIO

lit :: String -> T.ReadPrec String
lit = mapM (\c -> T.get >>= \c' -> 
    if toLower c' == toLower c then return c' else fail "")

