module XHotkey 
  ( module XHotkey.Types
  , module XHotkey.Core
  ) where  

import XHotkey.Types
import XHotkey.Core
import Data.MapTree

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
        ]
    , "3" .> forkX printBindings >> return ()
    , "4" .> spawn "urxvt -h" 
    ]
    

test x = runX' $ setBindings binds >> x
