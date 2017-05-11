{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens.TH
import           Data.Binary
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString.Lazy as BSL
import           Data.List
import qualified Data.Vector.Unboxed as V
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

data Register = R0
              | R1
              | R2
              | R3
              | R4
              | R5
              | R6
              | R7
              deriving (Eq, Show)

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

newtype Program = Program { _programWords :: V.Vector Word32 }
                deriving (Eq)
makeFields ''Program

instance Show Program where
  show (Program (V.null -> True)) = "Program []"
  show (Program vec) = "Program ["
                       ++ concat (intersperse ", " (map (`showHex` "") (V.toList (V.take 10 vec))))
                       ++ (if (V.null (V.drop 10 vec)) then "]" else ", ...]")

instance Binary Program where
  get = aux []
    where
      aux acc = do
        empty <- B.isEmpty
        if empty
          then return (Program (V.fromList (reverse acc)))
          else do word <- B.getWord32be
                  aux (word : acc)
  put (Program vec)
    | V.null vec = return ()
    | otherwise = do B.putWord32be (V.head vec)
                     put (Program (V.tail vec))
