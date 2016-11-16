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

import qualified Data.Map as M


data MChan m a = MChan_ (MVar (Either () (m a))) (MVar a) (IORef Bool)

newMChan :: (MonadIO m, MonadIO m') => m' (MChan m a)
newMChan = io $ do
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    running <- newIORef True
    return (MChan_ mv1 mv2 running)

closeMChan :: (MonadIO m, MonadIO m') => MChan m a -> m' ()
closeMChan (MChan_ v1 _ st) = io $ do
    running <- readIORef st
    if running then do
        putMVar v1 (Left ())
        writeIORef st False
    else do
        tryPutMVar v1 (Left ()) 
        return ()

evalMChan :: (MonadIO m, MonadIO m') => MChan m a -> m a -> m' a
evalMChan (MChan_ v1 v2 st) action = io $ do
    running <- readIORef st
    if running then do
        putMVar v1 (Right action)
        takeMVar v2 
    else
        error "MChan is closed."

runMChan :: (MonadIO m, MonadIO m') => MChan m a -> (m a -> m' a) -> m' Bool
runMChan (MChan_ mv1 mv2 st) handle = do
    running <- io $ readIORef st
    if running then do
        par <- io $ takeMVar mv1
        case par of
            Left _ -> return False
            Right par' -> do
                res <- handle par'
                io $ putMVar mv2 res
                return True
    else
        return False
