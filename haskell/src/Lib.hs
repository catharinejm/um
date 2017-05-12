module Lib where

import           Types
import           UM.Base

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Maybe
import           Data.Word
import           Numeric (showHex)

-- printLn :: (MonadIO io, Show a) => a -> io ()
-- printLn a = liftIO (print a >> putStrLn "")

runProgramFile :: (MonadIO io) => FilePath -> io ()
runProgramFile file = do
  bytes <- liftIO (BS.readFile file)
  res <- runProgram bytes
  case res of
   Left err -> liftIO $ do putStrLn ("**** ERROR: " ++ err)
                           exitFailure
   Right _ -> liftIO $ do putStrLn "Goodbye..."
                          exitSuccess

runProgram :: ByteString -> Either String ()
runProgram bytes = runExceptT (runReaderT (runST (initVM bytes >>= (\vm -> local (const vm) doit)) undefined))

doit :: (Program s m) => m ()
doit = return ()

initVM :: ByteString -> ST s (VM s)
initVM bytes = do
  r0 <- newRef 0
  r1 <- newRef 0
  r2 <- newRef 0
  r3 <- newRef 0
  r4 <- newRef 0
  r5 <- newRef 0
  r6 <- newRef 0
  r7 <- newRef 0
  ip <- newRef 0
  mem <- newArray 16 Nothing
  return $ VM r0 r1 r2 r3 r4 r5 r6 r7 ip mem

-- -- loadProgram :: (MonadIO m, Program m) => MemArray -> m ()
-- -- loadProgram ary = do
-- --   mempool %= (ix 0 .~ (Just ary))

r0, r1, r2, r3, r4, r5, r6, r7 :: (Program s m) => m (STRef s Word32)
r0 = asks vmR0
r1 = asks vmR1
r2 = asks vmR2
r3 = asks vmR3
r4 = asks vmR4
r5 = asks vmR5
r6 = asks vmR6
r7 = asks vmR7

setReg :: (Program s m) => Register -> Word32 -> m ()
setReg R0 val = r0 >>= flip writeRef val
setReg R1 val = r1 >>= flip writeRef val
setReg R2 val = r2 >>= flip writeRef val
setReg R3 val = r3 >>= flip writeRef val
setReg R4 val = r4 >>= flip writeRef val
setReg R5 val = r5 >>= flip writeRef val
setReg R6 val = r6 >>= flip writeRef val
setReg R7 val = r7 >>= flip writeRef val

getReg :: (Program s m) => Register -> m Word32
getReg R0 = r0 >>= readRef
getReg R1 = r1 >>= readRef
getReg R2 = r2 >>= readRef
getReg R3 = r3 >>= readRef
getReg R4 = r4 >>= readRef
getReg R5 = r5 >>= readRef
getReg R6 = r6 >>= readRef
getReg R7 = r7 >>= readRef

-- memArray :: (Program m) => Word32 -> m MemArray
-- memArray i = do mem <- use mempool
--                 case join (mem V.!? (fromIntegral i)) of
--                  Nothing -> throwError ("attempt to access unallocated MemArray: " ++ show i)
--                  Just ma -> return ma

-- newArray :: (Program m) => Word32 -> m Word32
-- newArray size = do
--   freeIdx <- findFreeMemSlot
--   mempool %= (ix freeIdx .~ Just (zeroedMemArray (fromIntegral size)))
--   return (fromIntegral freeIdx)
--   where 
--     findFreeMemSlot = do
--       midx <- uses mempool (V.findIndex isNothing)
--       case midx of
--        Nothing -> do size <- uses mempool V.length
--                      when (size >= truncate (2 ** 20)) $
--                        throwError ("too many allocations! c'mon man")
--                      mempool %= (V.++ V.replicate size Nothing)
--                      return size
--        Just i -> return i
   
-- dropArray :: (Program m) => Word32 -> m ()
-- dropArray 0 = throwError "Attempt to free array 0"
-- dropArray idx = do
--   mcur <- uses mempool (join . (V.!? (fromIntegral idx)))
--   case mcur of
--    Just _ -> mempool %= (ix (fromIntegral idx) .~ Nothing)
--    Nothing -> throwError ("Attempt to free unallocated array: " ++ show idx)

-- loadArray :: (Program m) => Word32 -> m ()
-- loadArray idx = do
--   ary <- memArray idx
--   mempool %= (ix 0 .~ Just ary)

-- getWord :: (Program m) => Word32 -> Word32 -> m Word32
-- getWord aryIdx offset = do
--   ary <- memArray aryIdx
--   case ary^.words ^? ix (fromIntegral offset) of
--    Nothing -> throwError ("index outside of MemArray bounds: " ++ show offset)
--    Just w -> return w

-- putWord :: (Program m) => Word32 -> Word32 -> Word32 -> m ()
-- putWord aryIdx offset val = do
--   _ <- getWord aryIdx offset -- throws if out of bounds
--   mempool %= (ix (fromIntegral aryIdx)._Just.words %~ (ix (fromIntegral offset) .~ val))

-- vmMVCOND :: (Program m) => VMInstruction -> m ()
-- vmMVCOND (VMInstruction _ regA regB regC) = do
--   cond <- getReg regC
--   when (cond /= 0) (getReg regB >>= setReg regA)
  
-- vmLOAD :: (Program m) => VMInstruction -> m ()
-- vmLOAD (VMInstruction _ regA regB regC) = do
--   aryIdx <- getReg regB
--   offset <- getReg regC
--   getWord aryIdx offset >>= setReg regA
  
-- vmSTORE :: (Program m) => VMInstruction -> m ()
-- vmSTORE (VMInstruction _ regA regB regC) = do
--   aryIdx <- getReg regA
--   offset <- getReg regB
--   val <- getReg regC
--   putWord aryIdx offset val

-- vmADD :: (Program m) => VMInstruction -> m ()
-- vmADD (VMInstruction _ regA regB regC) = do
--   b <- getReg regB
--   c <- getReg regC
--   setReg regA (b + c)

-- vmMULT :: (Program m) => VMInstruction -> m ()
-- vmMULT (VMInstruction _ regA regB regC) = do
--   b <- getReg regB
--   c <- getReg regC
--   setReg regA (b * c)

-- vmDIV :: (Program m) => VMInstruction -> m ()
-- vmDIV (VMInstruction _ regA regB regC) = do
--   b <- getReg regB
--   c <- getReg regC
--   setReg regA (b `div` c)

-- vmNAND :: (Program m) => VMInstruction -> m ()
-- vmNAND (VMInstruction _ regA regB regC) = do
--   b <- getReg regB
--   c <- getReg regC
--   setReg regA (complement (b .&. c))

-- vmHALT :: (MonadIO m, Program m) => VMInstruction -> m ()
-- vmHALT _ = liftIO exitSuccess

-- vmALLOC :: (Program m) => VMInstruction -> m ()
-- vmALLOC (VMInstruction _ regA regB regC) =
--   getReg regC >>= newArray >>= setReg regB

-- vmFREE :: (Program m) => VMInstruction -> m ()
-- vmFREE (VMInstruction _ regA regB regC) = do
--   getReg regC >>= dropArray

-- vmOUTPUT :: (MonadIO m, Program m) => VMInstruction -> m ()
-- vmOUTPUT (VMInstruction _ regA regB regC) = do
--   c <- getReg regC
--   when (c > 255) $ throwError ("invalid character: " ++ show c)
--   liftIO $ putChar (chr (fromIntegral c))

-- vmINPUT :: (MonadIO m, Program m) => VMInstruction -> m ()
-- vmINPUT (VMInstruction _ regA regB regC) = do
--   c <- liftIO getChar
--   setReg regC (fromIntegral (ord c))

-- vmLDPROG :: (Program m) => VMInstruction -> m ()
-- vmLDPROG (VMInstruction _ regA regB regC) = do
--   aryIdx <- getReg regB
--   newIp <- getReg regC  
--   ip .= newIp
--   if aryIdx == 0
--     then return ()
--     else do loadArray aryIdx


-- vmLDIMM :: (Program m) => VMSpecial -> m ()
-- vmLDIMM (VMSpecial _ reg imm) = do
--   setReg reg imm

-- decodeAndRun :: (MonadIO m, Program m) => m ()
-- decodeAndRun = do
--   insPtr <- use ip
--   insWord <- getWord 0 insPtr
--   let s = ((showHex insWord) "")
--       s' = replicate (8 - (length s)) '0' ++ s
--   liftIO $ putStrLn s'
--   case decodeInstruction insWord of
--    Nothing -> case decodeSpecial insWord of
--                Nothing -> failed insWord
--                Just spec -> vmLDIMM spec >> incIp >> decodeAndRun
--    Just ins -> case ins^.Types.op of
--                 MVCOND -> vmMVCOND ins >> incIp >> decodeAndRun
--                 LOAD -> vmLOAD ins >> incIp >> decodeAndRun
--                 STORE -> vmSTORE ins >> incIp >> decodeAndRun
--                 ADD -> vmADD ins >> incIp >> decodeAndRun
--                 MULT -> vmMULT ins >> incIp >> decodeAndRun
--                 DIV -> vmDIV ins >> incIp >> decodeAndRun
--                 NAND -> vmNAND ins >> incIp >> decodeAndRun
--                 HALT -> vmHALT ins >> incIp >> decodeAndRun
--                 ALLOC -> vmALLOC ins >> incIp >> decodeAndRun
--                 FREE -> vmFREE ins >> incIp >> decodeAndRun
--                 OUTPUT -> vmOUTPUT ins >> incIp >> decodeAndRun
--                 INPUT -> vmINPUT ins >> incIp >> decodeAndRun
--                 LDPROG -> vmLDPROG ins >> decodeAndRun
                
--   where
--     failed insWord = throwError ("failed to decode instruction: " ++ (showHex insWord) "")
--     incIp = ip += 1
