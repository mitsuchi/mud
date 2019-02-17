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
  import TypeEval

  pa :: String -> Either (ParseErrorBundle String Void) Expr
  pa program = parse topLevel "<stdin>" program
  
  te :: String -> IO ()
  te program = case pa program of
    Right expr -> runTypeEval expr
    Left bundle -> putStr (errorBundlePretty bundle)

  tef :: String -> IO ()
  tef file = do 
    program <- readFile file
    te program

  ev :: String -> IO ()
  ev program = case pa program of
    Right expr -> runEval expr
    Left bundle -> putStr (errorBundlePretty bundle)

  evalOnly :: String -> IO ()
  evalOnly program = case pa program of
    Right expr -> runEvalOnly expr
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
    insertPrimitives env
    expr' <- runExceptT (eval expr env)
    case expr' of 
      Right val -> putStrLn (show val)
      Left error -> putStrLn error

  runTypeCheckAndEval :: Expr -> IO String
  runTypeCheckAndEval expr = do
    env <- newIORef Map.empty
    insertPrimitives env
    typeSig <- runExceptT (typeEval expr env)
    case typeSig of
      Right val -> do
        env' <- newIORef Map.empty
        insertPrimitives env'
        expr' <- runExceptT (eval expr env')
        case expr' of 
          Right val -> return (show val)
          Left  error -> return error
      Left error -> return error    

  runEvalOnly :: Expr -> IO ()
  runEvalOnly expr = do
    env <- newIORef Map.empty
    insertPrimitives env
    expr' <- runExceptT (eval expr env)
    case expr' of 
      Right val -> putStr ""
      Left error -> putStrLn error

  runTypeEval :: Expr -> IO ()
  runTypeEval expr = do
    env <- newIORef Map.empty
    insertPrimitives env
    typeSig <- runExceptT (typeEval expr env)
    case typeSig of
      Right val -> putStrLn (show typeSig)
      Left error -> putStrLn error

  repl :: IO ()
  repl = do
    (newIORef Map.empty) >>= insertPrimitives >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

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

  help :: IO ()
  help = putStrLn $ "Mud is a functional programming language that supports multiple dispatch.\n"
    ++ "\n"
    ++ "Usage:\n"
    ++ "\n"
    ++ "        mud command [arguments]\n"
    ++ "\n"
    ++ "The commands are:\n"
    ++ "\n"
    ++ "        run     run Mud program\n"
    ++ "        eval    run one liner program\n"
    ++ "        repl    run read-eval-print loop\n"

  execParse :: [String] -> IO ()
  execParse ["parse", program] = case pa program of
    Right expr -> putStrLn (show expr)
    Left bundle -> putStrLn (errorBundlePretty bundle)

  parseEval :: String -> IO String
  parseEval program = do
    env <- newIORef Map.empty
    evalString env program

  parseTypeCheckEval :: String -> IO String
  parseTypeCheckEval program = do
    env <- newIORef Map.empty
    evalTypeCheckString env program    

  evalTypeCheckString :: Env -> String -> IO String
  evalTypeCheckString env program = case pa program of
    Right expr -> runTypeCheckAndEval expr
    Left bundle -> return (errorBundlePretty bundle)    
