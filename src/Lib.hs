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

  parseString :: String -> IOThrowsError Expr
  parseString program = case parseProgram program of
    Left bundle -> throwError $ errorBundlePretty bundle
    Right expr -> return expr

  -- REPLを実行する
  repl :: IO ()
  repl = do
    (newIORef Map.empty) >>= insertPrimitives >>= until_ (== "quit") (readPrompt "> ") . typeCheckAndEvalAndPrint

  -- 与えられた環境とプログラム文字列をもとに、型評価して評価して結果を表示する
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

  -- 条件が成り立つまでモナドアクションを繰り返す
  until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
  until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

  -- プロンプトを出しつつ、標準入力から一行を取得する
  readPrompt :: String -> IO String
  readPrompt prompt = flushStr prompt >> getLine      

  -- 文字列を出力し、標準出力をフラッシュする
  flushStr :: String -> IO ()
  flushStr str = putStr str >> hFlush stdout

  -- ヘルプを表示する
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

  -- プログラムをファイルから読み、型評価し、評価し、結果を表示する
  execRun :: [String] -> IO ()
  execRun ["run", filename] = do
    withFile filename ReadMode $ \handle -> do
      program <- hGetContents handle
      output <- parseTypeCheckEval program
      putStrLn output
  execRun ["run"] = putStrLn $ "mud run: run Mud program\n"
    ++ "\n"
    ++ "Usage:\n"
    ++ "\n"
    ++ "         mud run filepath\n"
    ++ "\n"
    ++ "Examples:\n"
    ++ "\n"
    ++ "         mud run example.mu\n" 

  -- プログラム文字列を型評価し、評価し、結果を表示する
  execEval :: [String] -> IO ()
  execEval ["eval", program] = do
    output <- parseTypeCheckEval program
    putStrLn output
  execEval ["eval"] = putStrLn $ "mud eval: run one-liner program\n"
    ++ "\n"
    ++ "Usage:\n"
    ++ "\n"
    ++ "         mud eval program\n"
    ++ "\n"
    ++ "Examples:\n"
    ++ "\n"
    ++ "         mud eval \"a=1;b=2;a+b\"\n"

  -- 以下は開発用のショートカット

  -- プログラムの文字列をパースしてエラーか式を返す
  pa :: String -> Either (ParseErrorBundle String Void) Expr
  pa program = parse topLevel "<stdin>" program
  
  -- プログラムの文字列をパースして型評価する
  te :: String -> IO ()
  te program = case pa program of
    Right expr -> do
      env <- newIORef Map.empty
      insertPrimitives env
      typeSig <- runExceptT (typeEval expr env)
      case typeSig of
        Right val -> putStrLn (show typeSig)
        Left error -> putStrLn error
    Left bundle -> putStr (errorBundlePretty bundle)

  -- ファイルからプログラムを読んでパースして、型評価する    
  tef :: String -> IO ()
  tef file = do 
    program <- readFile file
    te program

  -- プログラムの文字列をパースして評価して結果を表示する
  ev :: String -> IO ()
  ev program = do
    output <- runExceptT $ do
      expr <- parseString program
      output <- evalWithEmptyEnv expr
      return output
    case output of
      Left error -> putStrLn error
      Right expr -> putStrLn (show expr)  

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

  -- 空の環境で式を評価する
  evalWithEmptyEnv :: Expr -> IOThrowsError Expr
  evalWithEmptyEnv expr = do
    env <- liftIO $ newIORef Map.empty
    liftIO $ insertPrimitives env
    eval expr env
