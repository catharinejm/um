module UM.Base ( module BasePrelude
               , module Control.Monad.IO.Class
               , module Data.Array.IO
               , module UM.Base
               ) where

import qualified BasePrelude as Prelude
import           BasePrelude hiding (newIORef,
                                     readIORef,
                                     writeIORef,
                                     modifyIORef,
                                     print,
                                     putStrLn,
                                     putStr,
                                     exitFailure,
                                     exitSuccess)
import           Control.Monad.IO.Class
import           Data.Array.IO hiding (readArray,
                                       writeArray,
                                       newArray,
                                       newArray_,
                                       getBounds)

putStr :: (MonadIO io) => String -> io ()
putStr = liftIO . Prelude.putStr

putStrLn :: (MonadIO io) => String -> io ()
putStrLn = liftIO . Prelude.putStrLn

print :: (MonadIO io, Show a) => a -> io ()
print = liftIO . Prelude.print

printLn :: (MonadIO io, Show a) => a -> io ()
printLn = putStrLn . show

exitFailure :: (MonadIO io) => io ()
exitFailure = liftIO Prelude.exitFailure

exitSuccess :: (MonadIO io) => io ()
exitSuccess = liftIO Prelude.exitSuccess
