-- Main に直接関係するライブラリー
module Lib where

import           Control.Monad.Except
import qualified Control.Monad.Trans      as Trans
import           Data.IORef
import           Data.Map                 as Map hiding (foldr, take)
import           Data.Void
import qualified System.Console.Haskeline as Hline
import           System.IO
import           System.Timeout
import           Text.Megaparsec

import           Env
import           Eval
import           Expr
import           Parse
import           Primitive
import           RecList
import           TypeEval
import           TypeUtil

-- プログラムの文字列をパースしてエラーか式を返す
parseProgram :: String -> Either (ParseErrorBundle String Void) Expr
parseProgram program = parse topLevel "<stdin>" program

parseString :: String -> IOThrowsError Expr
parseString program = case parseProgram program of
  Left bundle -> throwError $ errorBundlePretty bundle
  Right expr  -> pure expr

-- REPLを実行する
repl :: IO ()
repl = Hline.runInputT Hline.defaultSettings $ do
  ref <- Trans.liftIO (newIORef Map.empty)
  env <- Trans.liftIO $ insertPrimitives ref
  loop env
  where
    loop :: Env -> Hline.InputT IO ()
    loop env  = do
      mayInput <- Hline.getInputLine "Mud>> "
      case mayInput of
        Nothing -> Hline.outputStrLn "Goodbye."
        Just input -> do
          Trans.liftIO $ typeCheckAndEvalAndPrint env input
          loop env
      -- until_ (== "quit") (readPrompt "> ") $ typeCheckAndEvalAndPrint env

-- 与えられた環境とプログラム文字列をもとに、型評価して評価して結果を表示する
typeCheckAndEvalAndPrint :: Env -> String -> IO ()
typeCheckAndEvalAndPrint env program = do
  output <- runExceptT $ do
    expr <- parseString program
    typeEvalWithEnv expr env
    eval expr env
  case output of
    Left error -> putStrLn error
    Right expr -> print expr

-- 条件が成り立つまでモナドアクションを繰り返す
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
 result <- prompt
 if pred result
    then pure ()
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
execParse programs =
  (\program ->
     case parseProgram program of
       Left bundle -> putStrLn (errorBundlePretty bundle)
       Right expr  -> print expr) `mapM_` programs

-- プログラムをファイルから読み、型評価し、評価する
execRun :: [String] -> IO ()
execRun [] = putStrLn $ "mud run: run Mud program\n"
  ++ "\n"
  ++ "Usage:\n"
  ++ "\n"
  ++ "         mud run filepath\n"
  ++ "\n"
  ++ "Examples:\n"
  ++ "\n"
  ++ "         mud run example.mu\n"
execRun ["limited", microSec, filename] =
  withFile filename ReadMode $ \handle -> do
  program <- hGetContents handle
  result  <- timeout (read microSec :: Int) $
    runExceptT $ do
    expr  <- parseString program
    _     <- typeEvalWithPrimitiveEnv expr
    evalWithPrimitiveEnv expr
  case result of
    Just (Left error) -> putStrLn error
    Just (Right expr) -> pure ()
    Nothing      -> putStrLn $ "runtime error: time limit exceeded ("
      ++ show ((read microSec :: Float)/10^6)
      ++ " sec)"
execRun [filename] =
  withFile filename ReadMode $ \handle -> do
  program <- hGetContents handle
  result <- runExceptT $ do
    expr <- parseString program
    typeEvalWithPrimitiveEnv expr
    evalWithPrimitiveEnv expr
  case result of
    Left error -> putStrLn error
    Right expr -> pure ()

-- プログラム文字列を型評価し、評価し、結果を表示する
execEval :: [String] -> IO ()
execEval [] = putStrLn $ "mud eval: run one-liner program\n"
  ++ "\n"
  ++ "Usage:\n"
  ++ "\n"
  ++ "         mud eval program\n"
  ++ "\n"
  ++ "Examples:\n"
  ++ "\n"
  ++ "         mud eval \"a=1;b=2;a+b\"\n"
execEval ["silent", "limited", microSec, program] = do
  result <- timeout (read microSec :: Int) $
    runExceptT $ do
    expr <- parseString program
    typeEvalWithPrimitiveEnv expr
    evalWithPrimitiveEnv expr
  case result of
    Just (Left error) -> putStrLn error
    Just (Right expr) -> pure ()
    Nothing      -> putStrLn $ "runtime error: time limit exceeded ("
     ++ show ((read microSec :: Float)/10^6)
     ++ " sec)"
execEval programs = do
  output <- ev `mapM` programs
  putStrLn `mapM_` output

-- 以下は開発用のショートカット

-- プログラムの文字列をパースしてエラーか式を返す
pa :: String -> Either (ParseErrorBundle String Void) Expr
pa program = parse topLevel "<stdin>" program

-- プログラムの文字列をパースして型評価する
te :: String -> IO String
te program = do
  output <- runExceptT $ do
    expr <- parseString program
    typeEvalWithPrimitiveEnv expr
  case output of
    Left error -> pure error
    Right expr -> pure $ show expr

-- ファイルからプログラムを読んでパースして、型評価する
tef :: String -> IO ()
tef file = do
  program <- readFile file
  output <- te program
  putStrLn output

-- プログラムの文字列をパースして型評価して評価して結果を表示する
ev :: String -> IO String
ev program = do
  output <- runExceptT $ do
    expr <- parseString program
    typeEvalWithPrimitiveEnv expr
    evalWithPrimitiveEnv expr
  case output of
    Left error -> pure error
    Right expr -> pure $ show expr

-- ファイルからプログラムを読んでパースして評価して結果を表示する
evf :: String -> IO ()
evf file = do
  program <- readFile file
  output <- ev program
  putStrLn output

-- ファイルからプログラムを読んでパースする
paf :: String -> IO ()
paf file = do
  program <- readFile file
  print $ pa program
