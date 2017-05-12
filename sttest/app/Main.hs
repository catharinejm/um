module Main where

import Control.Monad.ST
import Lib
import Data.Array.ST
import Data.Array.Base

main :: IO ()
main = do let ary = runSTArray someFunc
          putStrLn $ show (ary ! 5)
