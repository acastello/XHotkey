module XHotkey 
  ( module XHotkey.Types
  , module XHotkey.Core
  , module Graphics.X11.Types
  , module Graphics.X11.ExtraTypes
  , module Control.Monad.State
  , module Control.Monad.Reader
  ) where  

import Graphics.X11.Types
import Graphics.X11.ExtraTypes

import XHotkey.Types
import XHotkey.Core
import Data.NMap

import Data.Word

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader 


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
    

test x = runX $ setBindings binds' >> x
