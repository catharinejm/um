module UM.Base ( module BasePrelude
               , module Control.Monad.IO.Class
               , module Data.Array.IO
               ) where

import BasePrelude hiding (newIORef)
import Data.Array.IO hiding (writeArray, newArray, newArray_)
import Control.Monad.IO.Class
