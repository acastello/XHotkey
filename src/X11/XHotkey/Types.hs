{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XHotkey.Types 
    where

import qualified Data.MapTree as M
import Data.MapTree hiding (lookup)

import Control.Monad.Reader
import Control.Monad.State

import Graphics.X11
import Graphics.X11.Xlib.Extras (Event)

import Data.Word
import Data.Bits
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import qualified Text.Read as T
import Text.Read.Lex (numberToInteger)

infixl 7 .<. 
word .<. shift = shiftL word shift

infixl 7 .>. 
word .>. shift = shiftR word shift

-- | XEnv
data XEnv = XEnv
    { display   :: Display
    , rootWindow'   :: !Window
    , currentEvent :: XEventPtr
    , currentEventType :: EventType
    }

data XControl = XControl
    { hkMap :: Bindings
    , exitScheduled :: Bool
    }

type Bindings = M.MapTree KM (X ())
drawBindings :: Bindings -> String
drawBindings = drawMapTree

-- | X reader monad
newtype X a = X (ReaderT XEnv (StateT XControl IO) a)
    deriving (Functor, Applicative, Monad, MonadReader XEnv, MonadState XControl, MonadIO)

instance Show (X a) where
    show _ = "X ()"    

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
    show (KCode c) = "0x" ++ show c 
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

                kc <- T.choice 
                    [ do
                            str <- T.choice
                               [do 
                                    T.Ident str' <- T.lexP
                                    return str'
                               ,do
                                    n' <- T.readPrec :: T.ReadPrec Integer
                                    return (show n')]
                            let ks = stringToKeysym str
                            if ks == 0 then
                                fail "invalid keysym string"
                            else
                                return (KSym ks)
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

                onrel <- T.choice
                    [ do
                        '\'' <- T.get
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
                lit = mapM (\c -> T.get >>= \c' -> 
                    if toLower c' == toLower c then return c' else fail "")

instance Ord KM where
    KM u1 m1 k1 `compare` KM u2 m2 k2 = compare k1 k2 `mappend` compare m1 m2 `mappend` compare u1 u2

instance Enum KM where
    fromEnum (KM up st k) = ((fromEnum up) `shiftL` 59) .|. ((fromIntegral st) `shiftL` 27) .|. fromEnum k
    toEnum n = KM (testBit n 59) (fromIntegral $ n `shiftR` 27 .&. 0xfff) (toEnum $ n .&. 0x7ffffff)

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

