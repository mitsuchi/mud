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

execEval :: [String] -> IO ()
execEval ["eval", program] = ev program
execEval ["eval"] = putStrLn $ "mud eval: run one-liner program\n"
  ++ "\n"
  ++ "Usage:\n"
  ++ "\n"
  ++ "         mud eval program\n"
  ++ "\n"
  ++ "Examples:\n"
  ++ "\n"
  ++ "         mud eval \"a=1;b=2;a+b\"\n"

execRun :: [String] -> IO ()
execRun ["run", filename] = do
      withFile filename ReadMode $ \handle -> do
        contents <- hGetContents handle
        evalOnly contents
execRun ["run"] = putStrLn $ "mud run: run Mud program\n"
  ++ "\n"
  ++ "Usage:\n"
  ++ "\n"
  ++ "         mud run filepath\n"
  ++ "\n"
  ++ "Examples:\n"
  ++ "\n"
  ++ "         mud run example.mu\n"