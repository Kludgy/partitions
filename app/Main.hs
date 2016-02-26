module Main where

import Data.Partitions
import System.Environment

main :: IO ()
main = do
  [str] <- getArgs
  print (partitions str)
