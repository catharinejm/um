module Main where

import Lib
import System.Environment
import Types
import UM.Base
import System.IO (hSetBuffering, BufferMode(..), stdout)

main :: IO ()
main = do
  args <- getArgs
  liftIO $ hSetBuffering stdout NoBuffering
  case args of
   [file] -> runProgramFile file
   _ -> putStrLn "Usage: UM <file>"
