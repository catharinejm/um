module Lib where

import           Types

import qualified Data.Binary as B
import           Data.Word
import           System.IO

runProgram :: FilePath -> IO ()
runProgram file = do
  insxns <- B.decodeFile file :: IO Program
  print insxns
  putStrLn ""
  return ()
