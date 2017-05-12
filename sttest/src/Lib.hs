{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad.ST
import Data.Array.ST
import Data.List

data MyThing s = MyThing { ary :: STArray s Int Int }

printMyThing :: MyThing s -> ST s String
printMyThing (MyThing ary) = do
  elems <- getElems ary
  return $ "[" ++ concat (intersperse "," (map show elems)) ++ "]"

someFunc :: ST s (STArray s Int Int)
someFunc = do
  ary <- newListArray (1,10) [0..]
  return ary

