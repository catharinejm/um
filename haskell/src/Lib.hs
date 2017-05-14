module Lib where

import           Types
import           UM.Base
import           Util

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

runProgramFile :: (MonadIO io) => FilePath -> io ()
runProgramFile file = do
  bytes <- liftIO (BS.readFile file)
  res <- runExceptT (initVM bytes >>= \vm -> runReaderT runProgram vm)
  case res of
   Left err -> do putStrLn ("**** ERROR: " ++ err)
                  exitFailure
   Right _ -> do putStrLn "Goodbye..."
                 exitSuccess

initVM :: (MonadIO m) => ByteString -> ExceptT UMError m VM
initVM bytes = do
  let blen = BS.length bytes
  when (blen `rem` 4 /= 0) (throwError "Program must be a 4-byte aligned")
  ary <- newArray (i2w (blen `div` 4)) 0
  getWords bytes ary 0
  vm <- emptyVM
  mempool <- readIORef (vmMempool vm)
  writeArray mempool (i2w 0) (Just ary)
  return vm
  where
    getWords bs ary i = do
      if BS.null bs
        then return ary
        else do let (wdbs, rem) = BS.splitAt 4 bs
                    [b1, b2, b3, b4] = map b2w (BS.unpack wdbs)
                    word = (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4
                writeArray ary i word
                getWords rem ary (i+1)
    b2w :: Word8 -> Word32
    b2w = fromIntegral
    
r0, r1, r2, r3, r4, r5, r6, r7 :: (Program m) => m (IORef Word32)
r0 = asks vmR0
r1 = asks vmR1
r2 = asks vmR2
r3 = asks vmR3
r4 = asks vmR4
r5 = asks vmR5
r6 = asks vmR6
r7 = asks vmR7

getReg :: (Program m) => Register -> m Word32
getReg R0 = r0 >>= readIORef
getReg R1 = r1 >>= readIORef
getReg R2 = r2 >>= readIORef
getReg R3 = r3 >>= readIORef
getReg R4 = r4 >>= readIORef
getReg R5 = r5 >>= readIORef
getReg R6 = r6 >>= readIORef
getReg R7 = r7 >>= readIORef

setReg :: (Program m) => Register -> Word32 -> m ()
setReg R0 w = r0 >>= flip writeIORef w
setReg R1 w = r1 >>= flip writeIORef w
setReg R2 w = r2 >>= flip writeIORef w
setReg R3 w = r3 >>= flip writeIORef w
setReg R4 w = r4 >>= flip writeIORef w
setReg R5 w = r5 >>= flip writeIORef w
setReg R6 w = r6 >>= flip writeIORef w
setReg R7 w = r7 >>= flip writeIORef w

ip :: (Program m) => m (IORef Word32)
ip = asks vmIP

incIP :: (Program m) => m ()
incIP = ip >>= flip modifyIORef (+1)

setIP :: (Program m) => Word32 -> m ()
setIP w = ip >>= flip writeIORef w

getIP :: (Program m) => m Word32
getIP = ip >>= readIORef

mempoolRef :: (Program m) => m (IORef MemPool)
mempoolRef = asks vmMempool

mempool :: (Program m) => m MemPool
mempool = mempoolRef >>= readIORef

memArray :: (Program m) => Word32 -> m MemArray
memArray idx = do pool <- mempool
                  cap <- getCapacity pool
                  when (idx >= cap) badAccess
                  ary <- readArray pool idx
                  case ary of
                   Nothing -> badAccess
                   Just a -> return a
  where
    badAccess = throwError "Attempt to access unallocated array"
  
newMemArray :: (Program m) => Word32 -> m Word32
newMemArray size = do idx <- findFreeSlot 1
                      pool <- mempool
                      ary <- newArray size 0
                      writeArray pool idx (Just ary)
                      return idx
  where
    findFreeSlot i = do pool <- mempool
                        cap <- getCapacity pool
                        if (i >= cap)
                          then resizePool >> return i
                          else do mary <- readArray pool i
                                  case mary of
                                   Nothing -> return i
                                   Just _ -> findFreeSlot (i+1)
    resizePool = do ref <- mempoolRef
                    oldPool <- readIORef ref
                    oldCap <- getCapacity oldPool
                    newPool <- newArray (oldCap * 2) Nothing
                    copyArray newPool oldPool
                    writeIORef ref newPool

dropMemArray :: (Program m) => Word32 -> m ()
dropMemArray idx = do _ <- memArray idx -- will throw if out of bounds
                      pool <- mempool
                      writeArray pool idx Nothing

loadProgArray :: (Program m) => Word32 -> m ()
loadProgArray idx = do ary <- memArray idx
                       copy <- duplicateArray ary
                       pool <- mempool
                       writeArray pool 0 (Just copy)

getWord :: (Program m) => Word32 -> Word32 -> m Word32
getWord idx offset = do ary <- memArray idx
                        cap <- getCapacity ary
                        when (offset >= cap) $
                          throwError "Attempt to access index outside array bounds"
                        readArray ary offset

putWord :: (Program m) => Word32 -> Word32 -> Word32 -> m ()
putWord idx offset val = do _ <- getWord idx offset -- will throw if out of bounds
                            ary <- memArray idx
                            writeArray ary offset val

vmMVCOND :: (Program m) => VMInstruction -> m ()
vmMVCOND (VMInstruction _ regA regB regC) = do
  cond <- getReg regC
  when (cond /= 0) (getReg regB >>= setReg regA)
  
vmLOAD :: (Program m) => VMInstruction -> m ()
vmLOAD (VMInstruction _ regA regB regC) = do
  aryIdx <- getReg regB
  offset <- getReg regC
  getWord aryIdx offset >>= setReg regA
  
vmSTORE :: (Program m) => VMInstruction -> m ()
vmSTORE (VMInstruction _ regA regB regC) = do
  aryIdx <- getReg regA
  offset <- getReg regB
  val <- getReg regC
  putWord aryIdx offset val

vmADD :: (Program m) => VMInstruction -> m ()
vmADD (VMInstruction _ regA regB regC) = do
  b <- getReg regB
  c <- getReg regC
  setReg regA (b + c)

vmMULT :: (Program m) => VMInstruction -> m ()
vmMULT (VMInstruction _ regA regB regC) = do
  b <- getReg regB
  c <- getReg regC
  setReg regA (b * c)

vmDIV :: (Program m) => VMInstruction -> m ()
vmDIV (VMInstruction _ regA regB regC) = do
  b <- getReg regB
  c <- getReg regC
  setReg regA (b `div` c)

vmNAND :: (Program m) => VMInstruction -> m ()
vmNAND (VMInstruction _ regA regB regC) = do
  b <- getReg regB
  c <- getReg regC
  setReg regA (complement (b .&. c))

vmHALT :: (MonadIO m, Program m) => VMInstruction -> m ()
vmHALT _ = exitSuccess

vmALLOC :: (Program m) => VMInstruction -> m ()
vmALLOC (VMInstruction _ regA regB regC) =
  getReg regC >>= newMemArray >>= setReg regB

vmFREE :: (Program m) => VMInstruction -> m ()
vmFREE (VMInstruction _ regA regB regC) = do
  getReg regC >>= dropMemArray

vmOUTPUT :: (MonadIO m, Program m) => VMInstruction -> m ()
vmOUTPUT (VMInstruction _ regA regB regC) = do
  c <- getReg regC
  when (c > 255) $ throwError ("invalid character: " ++ show c)
  liftIO $ putChar (chr (fromIntegral c))

vmINPUT :: (MonadIO m, Program m) => VMInstruction -> m ()
vmINPUT (VMInstruction _ regA regB regC) = do
  c <- liftIO getChar
  setReg regC (fromIntegral (ord c))

vmLDPROG :: (Program m) => VMInstruction -> m ()
vmLDPROG (VMInstruction _ regA regB regC) = do
  aryIdx <- getReg regB
  newIp <- getReg regC  
  setIP newIp
  if aryIdx == 0
    then return ()
    else loadProgArray aryIdx

vmLDIMM :: (Program m) => VMSpecial -> m ()
vmLDIMM (VMSpecial _ reg imm) = do
  setReg reg imm

runProgram :: (Program m) => m ()
runProgram = do
  insPtr <- getIP
  insWord <- getWord 0 insPtr
  -- let s = ((showHex insWord) "")
  --     s' = replicate (8 - (length s)) '0' ++ s
  -- liftIO $ putStrLn s'
  case decodeInstruction insWord of
   Nothing -> case decodeSpecial insWord of
               Nothing -> failed insWord
               Just spec -> vmLDIMM spec >> incIP >> runProgram
   Just ins -> case ins^.Types.op of
                MVCOND -> vmMVCOND ins >> incIP >> runProgram
                LOAD -> vmLOAD ins >> incIP >> runProgram
                STORE -> vmSTORE ins >> incIP >> runProgram
                ADD -> vmADD ins >> incIP >> runProgram
                MULT -> vmMULT ins >> incIP >> runProgram
                DIV -> vmDIV ins >> incIP >> runProgram
                NAND -> vmNAND ins >> incIP >> runProgram
                HALT -> vmHALT ins >> incIP >> runProgram
                ALLOC -> vmALLOC ins >> incIP >> runProgram
                FREE -> vmFREE ins >> incIP >> runProgram
                OUTPUT -> vmOUTPUT ins >> incIP >> runProgram
                INPUT -> vmINPUT ins >> incIP >> runProgram
                LDPROG -> vmLDPROG ins >> runProgram
                
  where
    failed insWord = throwError ("failed to decode instruction: " ++ (showHex insWord) "")
