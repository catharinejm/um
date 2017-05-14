module Util where

import qualified Data.Array.IO as AIO
import qualified Data.IORef as IORef
import           UM.Base

w2i :: (Integral i, Ix i) => Word32 -> i
w2i = fromIntegral

i2w :: (Integral i) => i -> Word32
i2w = fromIntegral

newIORef :: (MonadIO m) => e -> m (IORef e)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef e -> m e
readIORef = liftIO . IORef.readIORef

writeIORef :: (MonadIO m) => IORef e -> e -> m ()
writeIORef ref val = liftIO (IORef.writeIORef ref val)

modifyIORef :: (MonadIO m) => IORef e -> (e -> e) -> m ()
modifyIORef ref f = liftIO (IORef.modifyIORef ref f)

type IOArrayOp a i e m = (MonadIO m, MArray a e IO, Integral i, Ix i)

_range :: (Integral i) => i -> (i, i)
_range size = (0, size-1)

newArray :: (IOArrayOp a i e m) => i -> e -> m (a i e)
newArray size init = liftIO (AIO.newArray (_range size) init)

newArray_ :: (IOArrayOp a i e m) => i -> m (a i e)
newArray_ size = liftIO $ AIO.newArray_ (_range size)

writeArray :: (IOArrayOp a i e m) => a i e -> i -> e -> m ()
writeArray ary i el = liftIO (AIO.writeArray ary i el)

readArray :: (IOArrayOp a i e m) => a i e -> i -> m e
readArray ary i = liftIO (AIO.readArray ary i)

getBounds :: (IOArrayOp a i e m) => a i e -> m (i, i)
getBounds ary = liftIO (AIO.getBounds ary)

getCapacity :: (IOArrayOp a i e m) => a i e -> m i
getCapacity ary = getBounds ary >>= return . (+1) . snd

copyArray :: (IOArrayOp a i e m) => a i e -> a i e -> m ()
copyArray dest src = do destCap <- getCapacity dest
                        srcCap <- getCapacity src
                        let copyMax = destCap `min` srcCap
                        doCopy 0 copyMax
  where
    doCopy i max = do if i >= max
                        then return ()
                        else do elem <- readArray src i
                                writeArray dest i elem
                                doCopy (i+1) max
                         
duplicateArray :: (IOArrayOp a i e m) => a i e -> m (a i e)
duplicateArray ary = do cap <- getCapacity ary
                        newAry <- newArray_ cap
                        copyArray newAry ary
                        return newAry
