module Util where

import qualified Data.Array.IO as AIO
import qualified Data.IORef as IORef
import           UM.Base

newIORef :: (MonadIO m) => e -> m (IORef e)
newIORef = liftIO . IORef.newIORef

newArray :: (MonadIO m, MArray a e IO, Ix i) => (i, i) -> e -> m (a i e)
newArray range init = liftIO (AIO.newArray range init)

newArray_ :: (MonadIO m, MArray a e IO, Ix i) => (i, i) -> m (a i e)
newArray_ = liftIO . AIO.newArray_

writeArray :: (MonadIO m, MArray a e IO, Ix i) => a i e -> i -> e -> m ()
writeArray ary i el = liftIO (AIO.writeArray ary i el)
