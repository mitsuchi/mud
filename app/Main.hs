module Main where

import System.Environment (getArgs)
import System.IO
import Lib

main :: IO ()
main = do
  args <- getArgs
  if args == [] then help
    else case args !! 0 of 
      "eval" -> execEval args
      "parse" -> execParse args
      "run"  -> execRun args
      "repl" -> repl
      _      -> help
