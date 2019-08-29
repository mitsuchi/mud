module Main where

import           Lib
import qualified Safe
import           System.Environment (getArgs)
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  case Safe.atDef "help" args 0 of
    "eval"  -> execEval  (drop 1 args)
    "parse" -> execParse (drop 1 args)
    "run"   -> execRun   (drop 1 args)
    "repl"  -> repl
    _       -> help
