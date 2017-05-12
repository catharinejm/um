{-# LANGUAGE TemplateHaskell #-}

module Types where

import           UM.Base

import           Control.Lens.TH
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Array.ST (STArray, STUArray)
import qualified Data.Array.ST as STA
import           Data.Bits
import qualified Data.ByteString.Lazy as BSL
import           Data.List
import           Data.Word
import           Numeric (showHex)

data Op = MVCOND
        | LOAD
        | STORE
        | ADD
        | MULT
        | DIV
        | NAND
        | HALT
        | ALLOC
        | FREE
        | OUTPUT
        | INPUT
        | LDPROG
        | LDIMM
        deriving (Eq, Show)

decodeOp :: Word32 -> Op
decodeOp = aux . (`shiftR` 28)
  where
    aux  0 = MVCOND
    aux  1 = LOAD
    aux  2 = STORE
    aux  3 = ADD
    aux  4 = MULT
    aux  5 = DIV
    aux  6 = NAND
    aux  7 = HALT
    aux  8 = ALLOC
    aux  9 = FREE
    aux 10 = OUTPUT
    aux 11 = INPUT
    aux 12 = LDPROG
    aux 13 = LDIMM
    aux  o = error ("invalid opcode: " ++ show o)

data Register = R0
              | R1
              | R2
              | R3
              | R4
              | R5
              | R6
              | R7
              deriving (Eq, Show)

decodeRegister :: Word32 -> Register
decodeRegister 0 = R0
decodeRegister 1 = R1
decodeRegister 2 = R2
decodeRegister 3 = R3
decodeRegister 4 = R4
decodeRegister 5 = R5
decodeRegister 6 = R6
decodeRegister 7 = R7
decodeRegister r = error ("invalid register number: " ++ show r)

data VMInstruction = VMInstruction { _vMInstructionOp :: Op
                                   , _vMInstructionRegA :: Register
                                   , _vMInstructionRegB :: Register
                                   , _vMInstructionRegC :: Register
                                   }
makeFields ''VMInstruction

data VMSpecial = VMSpecial { _vMSpecialOp :: Op
                           , _vMSpecialRegA :: Register
                           , _vMSpecialImmed :: Word32
                           }
               deriving (Eq, Show)
makeFields ''VMSpecial

decodeInstruction :: Word32 -> Maybe VMInstruction
decodeInstruction (decodeOp -> LDIMM) = Nothing
decodeInstruction word@(decodeOp -> op) =
  Just $ VMInstruction op regA regB regC
  where
    regA = decodeRegister $ (word `shiftR` 6) .&. 0x7
    regB = decodeRegister $ (word `shiftR` 3) .&. 0x7
    regC = decodeRegister $ word .&. 0x7

decodeSpecial :: Word32 -> Maybe VMSpecial
decodeSpecial word@(decodeOp -> op@LDIMM) = Just $ VMSpecial op regA immed
  where
    regA = decodeRegister $ (word `shiftR` 25) .&. 0x7
    immed = word .&. 0x1FFFFFF
decodeSpecial _ = Nothing

type MemArray s = STUArray s Int Word32

data VM s = VM { vmR0 :: STRef s Word32
               , vmR1 :: STRef s Word32
               , vmR2 :: STRef s Word32
               , vmR3 :: STRef s Word32
               , vmR4 :: STRef s Word32
               , vmR5 :: STRef s Word32
               , vmR6 :: STRef s Word32
               , vmR7 :: STRef s Word32
               , vmIP :: STRef s Word32
               , vmMempool :: STArray s Int (Maybe (MemArray s))
               }

data ShowVM = ShowVM { showVMR0 :: Word32
                     , showVMR1 :: Word32
                     , showVMR2 :: Word32
                     , showVMR3 :: Word32
                     , showVMR4 :: Word32
                     , showVMR5 :: Word32
                     , showVMR6 :: Word32
                     , showVMR7 :: Word32
                     , showVMIP :: Word32
                     }
            deriving (Show)

showableVM :: VM s -> ST s ShowVM
showableVM (VM r0 r1 r2 r3 r4 r5 r6 r7 ip _) = do
  r0' <- readSTRef r0
  r1' <- readSTRef r1
  r2' <- readSTRef r2
  r3' <- readSTRef r3
  r4' <- readSTRef r4
  r5' <- readSTRef r5
  r6' <- readSTRef r6
  r7' <- readSTRef r7
  ip' <- readSTRef ip
  return $ ShowVM r0' r1' r2' r3' r4' r5' r6' r7' ip'

type UMError = String
type Program s m = (MonadReader (VM s) m, MonadError UMError m, MonadST s m)
type ProgramST s = ExceptT UMError (ReaderT (VM s) (ST s))

class MonadST s m where
  newRef :: a -> m (STRef s a)
  readRef :: STRef s a -> m a
  writeRef :: STRef s a -> a -> m ()
  newArray :: Int -> a -> m (STArray s Int a)

instance MonadST s (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef r a = writeSTRef r a
  newArray size a = STA.newArray (0, size-1) a

instance MonadST s (ProgramST s) where
  newRef = lift . lift . newSTRef
  readRef = lift . lift . readSTRef
  writeRef r a = lift . lift $ writeSTRef r a
  newArray size a = lift . lift $ STA.newArray (0, size-1) a
