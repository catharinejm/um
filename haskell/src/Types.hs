{-# LANGUAGE TemplateHaskell #-}

module Types where

import           UM.Base
import           Util

import           Control.Lens.TH
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Array.IO as AIO
import           Data.Bits
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IORef as IORef
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

type MemArray = IOUArray Word32 Word32

type MemPool = IOArray Word32 (Maybe MemArray)

data VM = VM { vmR0 :: IORef Word32
             , vmR1 :: IORef Word32
             , vmR2 :: IORef Word32
             , vmR3 :: IORef Word32
             , vmR4 :: IORef Word32
             , vmR5 :: IORef Word32
             , vmR6 :: IORef Word32
             , vmR7 :: IORef Word32
             , vmIP :: IORef Word32
             , vmMempool :: IORef MemPool
             }

emptyVM :: (MonadIO io) => io VM
emptyVM = do [r0, r1, r2, r3, r4, r5, r6, r7, ip] <- replicateM 9 (newIORef 0)
             mempool <- newArray 16 Nothing >>= newIORef
             return $ VM r0 r1 r2 r3 r4 r5 r6 r7 ip mempool

type UMError = String
type Program m = (MonadIO m, MonadError UMError m, MonadReader VM m)
type ProgramST m = ReaderT VM (ExceptT UMError m)
