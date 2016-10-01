module XHotkey 
  ( module XHotkey.Types
  , module XHotkey.Core
  ) where  

import XHotkey.Types
import XHotkey.Core
import Data.MapTree

import System.Random

import Data.Word

import Control.Monad.State


data D = A Int | B Word16 | C Word32
    deriving (Show)

binds :: Bindings
binds = mapKeys read $ fromList
    [ "q" .> exitX
    , "1" .> liftIO $ print 1
    , "2" .<
        [ "1" .> liftIO $ print 12
        , "2" .> liftIO $ print 22
        , "btn1-1" .> io $ print "btn1-1"
        ]
    , "3" .> forkX printBindings >> return ()
    , "4" .> spawn "urxvt -title floating_urxvt" 
    , "k" .> io $ do
        n <- randomRIO (5,20) 
        putStr =<< replicateM n (randomRIO (False, True) >>= \f -> return $ if f then 'K' else 'κ')
        return ()
    , "mod5-1" .> io$ print "mod5-1"
    , "c" .> setBindings binds' >> printBindings >> return ()
    ]

binds' :: Bindings
binds' = mapKeys read $ fromList
    [ "q" .> exitX
    , "c" .> setBindings binds >> printBindings >> return ()
    , "1" .> liftIO $ print 2
    , "mouse3" .> liftIO $ putStrLn "mouse3"
    ]
    

test x = runX' $ setBindings binds' >> x
