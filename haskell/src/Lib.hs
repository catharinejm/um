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

-- printLn :: (MonadIO io, Show a) => a -> io ()
-- printLn a = liftIO (print a >> putStrLn "")

runProgramFile :: (MonadIO io) => FilePath -> io ()
runProgramFile file = do
  bytes <- liftIO (BS.readFile file)
  res <- runExceptT (initVM bytes >>= \vm -> runReaderT runProgram vm)
  case res of
   Left err -> liftIO $ do putStrLn ("**** ERROR: " ++ err)
                           exitFailure
   Right _ -> liftIO $ do putStrLn "Goodbye..."
                          exitSuccess

initVM :: (MonadIO m) => ByteString -> ExceptT UMError m VM
initVM bytes = do
  let blen = BS.length bytes
  when (blen `rem` 4 /= 0) (throwError "Program must be a 4-byte aligned")
  ary <- newArray (0, (blen `div` 4) - 1) 0
  getWords bytes ary 0
  mempool <- newArray_ (0, 15)
  writeArray mempool 0 (Just ary)
  r0 <- newIORef 0
  r1 <- newIORef 0
  r2 <- newIORef 0
  r3 <- newIORef 0
  r4 <- newIORef 0
  r5 <- newIORef 0
  r6 <- newIORef 0
  r7 <- newIORef 0
  ip <- newIORef 0
  return $ VM r0 r1 r2 r3 r4 r5 r6 r7 ip mempool
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
    
runProgram :: (Program m) => m ()
runProgram = undefined

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
