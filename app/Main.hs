module Main where

import System.IO
import Lib

main :: IO ()
main = do
  input <- getContents
  ev input
