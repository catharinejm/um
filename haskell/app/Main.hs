module Main where

import Lib
import Types
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
   [file] -> runProgramFile file
   _ -> putStrLn "Usage: UM <file>"
