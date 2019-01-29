module Lib where

  import Control.Monad.Except
  import Data.Void
  import Data.IORef
  import Data.Map as Map hiding (map, foldr, take)
  import System.IO
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
    expr' <- runExceptT (eval expr env)
    case expr' of 
      Right val -> print val
      Left error -> print error

  repl :: IO ()
  repl = do
    (newIORef Map.empty) >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

  evalAndPrint :: Env -> String -> IO ()
  evalAndPrint env expr =  evalString env expr >>= putStrLn

  evalString :: Env -> String -> IO String
  evalString env program = case pa program of
    Right expr -> do 
      expr' <- runExceptT (eval expr env)
      case expr' of 
        Right val -> return (show val)
        Left error -> return error
    Left bundle -> return (errorBundlePretty bundle)

  until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
  until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

  readPrompt :: String -> IO String
  readPrompt prompt = flushStr prompt >> getLine      

  flushStr :: String -> IO ()
  flushStr str = putStr str >> hFlush stdout
