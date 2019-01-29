module Main where

import System.Environment (getArgs)
import System.IO
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["eval", program] -> do
      ev program
    ["go", filename] -> do
      withFile filename ReadMode $ \handle -> do
        contents <- hGetContents handle
        ev contents
    ["repl"] -> do
      repl
          