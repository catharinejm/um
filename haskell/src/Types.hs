{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Lens.TH
import           Control.Monad.Except (MonadError)
import           Control.Monad.State.Strict (MonadState)
import           Data.Binary
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import           Data.Bits
import qualified Data.ByteString.Lazy as BSL
import           Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
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

data VMInstruction = VMInstruction { _vMInstructionOp :: !Op
                                   , _vMInstructionRegA :: !Register
                                   , _vMInstructionRegB :: !Register
                                   , _vMInstructionRegC :: !Register
                                 }
makeFields ''VMInstruction

data VMSpecial = VMSpecial { _vMSpecialOp :: !Op
                           , _vMSpecialRegA :: !Register
                           , _vMSpecialImmed :: !Word32
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

newtype MemArray = MemArray { _memArrayWords :: VU.Vector Word32 }
                deriving (Eq, Monoid)
makeFields ''MemArray

emptyMemArray :: MemArray
emptyMemArray = mempty

zeroedMemArray :: Int -> MemArray
zeroedMemArray size = MemArray (VU.replicate size 0)

instance Show MemArray where
  show (MemArray (VU.null -> True)) = "MemArray []"
  show (MemArray vec) = "MemArray ["
                       ++ concat (intersperse ", " (map (`showHex` "") (VU.toList (VU.take 10 vec))))
                       ++ (if (VU.null (VU.drop 10 vec)) then "]" else ", ...]")

instance Binary MemArray where
  get = aux []
    where
      aux acc = do
        empty <- B.isEmpty
        if empty
          then return (MemArray (VU.fromList (reverse acc)))
          else do word <- B.getWord32be
                  aux (word : acc)
  put (MemArray vec)
    | VU.null vec = return ()
    | otherwise = do B.putWord32be (VU.head vec)
                     put (MemArray (VU.tail vec))

data VM = VM { _r0 :: !Word32
             , _r1 :: !Word32
             , _r2 :: !Word32
             , _r3 :: !Word32
             , _r4 :: !Word32
             , _r5 :: !Word32
             , _r6 :: !Word32
             , _r7 :: !Word32
             , _ip :: !Word32
             , _mempool :: !(V.Vector (Maybe MemArray))
             }
        deriving (Show)
makeClassy ''VM

initVM :: VM
initVM = VM 0 0 0 0 0 0 0 0 0 (V.fromList (replicate 16 Nothing))

type Program m = (MonadState VM m, MonadError String m)
