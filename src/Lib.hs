module Lib where

  import Data.Void
  import Data.IORef
  import Data.Map as Map hiding (map, foldr, take)
  import Text.Megaparsec
  
  import DeepList
  import Env  
  import Expr
  import Eval
  import Parse
  import Primitive
  import TypeUtil
  
  parseExpr :: String -> Expr
  parseExpr program = case parse expr "<stdin>" program of
    Right expr -> expr
    Left bundle -> error (errorBundlePretty bundle)  

  pa :: String -> Either (ParseErrorBundle String Void) Expr
  pa program = parse topLevel "<stdin>" program
  
  ev :: String -> IO ()
  ev program = case pa program of
    Right expr -> runEval expr
    Left bundle -> putStr (errorBundlePretty bundle)
  
  evf :: String -> IO ()
  evf file = do 
    program <- readFile file
    ev program
  
  paf :: String -> IO ()
  paf file = do 
    program <- readFile file
    print $ pa program
  
  runEval :: Expr -> IO ()
  runEval expr = do
    env <- newIORef Map.empty
    expr' <- eval expr env
    print expr'  
