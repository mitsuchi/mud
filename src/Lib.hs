module Lib where

  import Control.Monad (void, forM_)
  import Control.Monad.Combinators.Expr
  import Data.Void
  import Data.Char
  import Data.IORef
  import Data.List (find, intercalate, isInfixOf)
  import Data.Map as Map hiding (map, foldr, take)
  import Debug.Trace
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L
  
  import DeepList
  import Env  
  import Expr
  import Eval
  import Parse
  import Primitive
  import TypeUtil

  fromVars :: [Expr] -> [String]
  fromVars (Var v:[]) = [v]
  fromVars (Var v:es) = v : fromVars es
  fromVars (e:es) = fromVars es
  
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

  showResult :: Either String Env -> IO ()
  showResult res = case res of
    Left message -> print message
    Right env -> showEnv env
