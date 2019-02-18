module Lib where
  
  import Control.Monad.Except
  import Data.Void
  import Data.IORef
  import Data.Map as Map hiding (foldr, take)
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

  -- プログラムの文字列をパースしてエラーか式を返す
  parseProgram :: String -> Either (ParseErrorBundle String Void) Expr
  parseProgram program = parse topLevel "<stdin>" program
    
  -- プログラムの文字列をパースしてエラーか式を返す
  pa :: String -> Either (ParseErrorBundle String Void) Expr
  pa program = parse topLevel "<stdin>" program
  
  -- プログラムの文字列をパースして型評価する
  te :: String -> IO ()
  te program = case pa program of
    Right expr -> runTypeEval expr
    Left bundle -> putStr (errorBundlePretty bundle)

  -- ファイルからプログラムを読んでパースして、型評価する    
  tef :: String -> IO ()
  tef file = do 
    program <- readFile file
    te program

  -- プログラムの文字列をパースして評価して結果を表示する
  ev :: String -> IO ()
  ev program = case pa program of
    Right expr -> runEval expr
    Left bundle -> putStr (errorBundlePretty bundle)

  -- プログラムの文字列をパースして評価して結果を表示しない
  evalOnly :: String -> IO ()
  evalOnly program = case pa program of
    Right expr -> runEvalOnly expr
    Left bundle -> putStr (errorBundlePretty bundle)

  -- ファイルからプログラムを読んでパースして評価して結果を表示する
  evf :: String -> IO ()
  evf file = do 
    program <- readFile file
    ev program
  
  -- ファイルからプログラムを読んでパースする
  paf :: String -> IO ()
  paf file = do 
    program <- readFile file
    print $ pa program
  
  -- 式を評価して結果を表示する
  runEval :: Expr -> IO ()
  runEval expr = do
    env <- newIORef Map.empty
    insertPrimitives env
    expr' <- runExceptT (eval expr env)
    case expr' of 
      Right val -> putStrLn (show val)
      Left error -> putStrLn error

  -- 式を型評価してOKなら評価して結果をIO文字列で返す
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

  -- 式を評価して結果を表ししない
  runEvalOnly :: Expr -> IO ()
  runEvalOnly expr = do
    env <- newIORef Map.empty
    insertPrimitives env
    expr' <- runExceptT (eval expr env)
    case expr' of 
      Right val -> putStr ""
      Left error -> putStrLn error

  -- 式を型評価して結果を表示する
  runTypeEval :: Expr -> IO ()
  runTypeEval expr = do
    env <- newIORef Map.empty
    insertPrimitives env
    typeSig <- runExceptT (typeEval expr env)
    case typeSig of
      Right val -> putStrLn (show typeSig)
      Left error -> putStrLn error

  -- REPLを実行する
  repl :: IO ()
  repl = do
    (newIORef Map.empty) >>= insertPrimitives >>= until_ (== "quit") (readPrompt "> ") . typeCheckAndEvalAndPrint

  typeCheckAndEvalAndPrint :: Env -> String -> IO ()
  typeCheckAndEvalAndPrint env program = do
    case parseProgram program of
      Left bundle -> putStrLn (errorBundlePretty bundle)        
      Right expr -> do
        varMap <- readIORef env
        env' <- newIORef (Map.map ( \xs -> Prelude.map (\(types, expr) -> (types, (TypeLit . typeOf') expr)) xs ) varMap)
        insertPrimitives env'
        typeSig <- runExceptT (typeEval expr env')
        case typeSig of
          Left error -> putStrLn error
          Right val -> do
            expr' <- runExceptT (eval expr env)
            case expr' of 
              Left  error -> putStrLn error                
              Right val -> putStrLn (show val)

  -- 与えられた環境で式を評価して結果を表示する
  evalAndPrint :: Env -> String -> IO ()
  evalAndPrint env expr =  evalString env expr >>= putStrLn

  -- プログラム文字列をパースし、与えられた環境で式を評価して結果をIO文字列か返す
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

  -- パースを実行する
  execParse :: [String] -> IO ()
  execParse ["parse", program] =
    case parseProgram program of
      Left bundle -> putStrLn (errorBundlePretty bundle)        
      Right expr -> putStrLn (show expr)

  -- プログラム文字列をパースして評価して結果をIO文字列で返す
  parseEval :: String -> IO String
  parseEval program = do
    env <- newIORef Map.empty
    evalString env program

  -- プログラム文字列をパースして型評価して結果をIO文字列で返す  
  parseTypeCheckEval :: String -> IO String
  parseTypeCheckEval program = 
      case parseProgram program of
        Left bundle -> return $ (errorBundlePretty bundle)        
        Right expr -> do
          env <- newIORef Map.empty
          insertPrimitives env
          typeSig <- runExceptT (typeEval expr env)
          case typeSig of
            Left error -> return $ error
            Right val -> do
              env' <- newIORef Map.empty
              insertPrimitives env'
              expr' <- runExceptT (eval expr env')
              case expr' of 
                Left  error -> return error                
                Right val -> return (show val)

  -- プログラム文字列をパースして、与えられた環境で型評価して結果をIO文字列で返す      
  evalTypeCheckString :: Env -> String -> IO String
  evalTypeCheckString env program = case pa program of
    Right expr -> runTypeCheckAndEval expr
    Left bundle -> return (errorBundlePretty bundle)    

  execRun :: [String] -> IO ()
  execRun ["run", filename] = do
    withFile filename ReadMode $ \handle -> do
      program <- hGetContents handle
      case parseProgram program of
        Left bundle -> putStrLn (errorBundlePretty bundle)        
        Right expr -> do
          env <- newIORef Map.empty
          insertPrimitives env
          typeSig <- runExceptT (typeEval expr env)
          case typeSig of
            Left error -> putStrLn error
            Right val -> do
              env' <- newIORef Map.empty
              insertPrimitives env'
              expr' <- runExceptT (eval expr env')
              case expr' of 
                Left  error -> putStrLn error                
                Right val -> putStr ""
  execRun ["run"] = putStrLn $ "mud run: run Mud program\n"
    ++ "\n"
    ++ "Usage:\n"
    ++ "\n"
    ++ "         mud run filepath\n"
    ++ "\n"
    ++ "Examples:\n"
    ++ "\n"
    ++ "         mud run example.mu\n"

  tryEither :: (Either a b) -> (b -> (Either a b)) -> Either a b
  tryEither x f = case x of
    Left l -> Left l
    Right r -> f r    

  execEval :: [String] -> IO ()
  execEval ["eval", program] = 
      case parseProgram program of
        Left bundle -> putStrLn (errorBundlePretty bundle)        
        Right expr -> do
          env <- newIORef Map.empty
          insertPrimitives env
          typeSig <- runExceptT (typeEval expr env)
          case typeSig of
            Left error -> putStrLn error
            Right val -> do
              env' <- newIORef Map.empty
              insertPrimitives env'
              expr' <- runExceptT (eval expr env')
              case expr' of 
                Left  error -> putStrLn error                
                Right val -> putStrLn (show val)
  execEval ["eval"] = putStrLn $ "mud eval: run one-liner program\n"
    ++ "\n"
    ++ "Usage:\n"
    ++ "\n"
    ++ "         mud eval program\n"
    ++ "\n"
    ++ "Examples:\n"
    ++ "\n"
    ++ "         mud eval \"a=1;b=2;a+b\"\n"