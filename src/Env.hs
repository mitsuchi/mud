-- 環境
module Env where

import           Data.IORef
import           Data.Map    as Map
import           Debug.Trace
import           RecList
import           TypeUtil


type GeneralEnv a = IORef (Map String [(RecList String, a)])

-- 与えられた名前の変数を探す
lookupVar :: (Show a) => String -> GeneralEnv a -> IO (Maybe a)
lookupVar name env = lookupVar' name env False

-- 与えられた名前の変数を探す。ただし変数として名前がなくても関数として一つだけあるならそれを返す
lookupVarLoose :: (Show a) => String -> GeneralEnv a -> IO (Maybe a)
lookupVarLoose name env = lookupVar' name env True

-- 与えられた名前の変数を探す。上2つを統合した関数。
lookupVar' :: (Show a) => String -> GeneralEnv a -> Bool -> IO (Maybe a)
lookupVar' name env loose = do
  env' <- readIORef env
  pure $ do
    vars <- Map.lookup name env'
    case vars of
      -- 同じ名前の定義の先頭に変数があればそれを参照する
      (Elem "_", expr):es -> Just expr
      -- 変数として名前がなくても関数として1つだけ名前があるならそれを参照する
      (Elems _, expr):[]  -> if loose then Just expr else Nothing
      -- そうでなければなし
      otherwise           -> Nothing

-- 関数を環境に登録する
-- 同名かつ同型の関数がない場合のみ登録できる
insertFun :: (Show a) => String -> RecList String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
insertFun name types expr env = do
  fe <- funExists name types env
  if not fe
  then do
    insertFun' name types expr env
    pure $ Right env
  else
    pure $ Left ("function '" ++ name ++ " : " ++ argSig types ++ "' already exists")

-- 同名の関数がある場合は、具体型の関数は先頭に、多相型の関数は末尾に追加する
insertFun' :: String -> RecList String -> a -> GeneralEnv a -> IO (GeneralEnv a)
insertFun' name types expr env = do
  env' <- readIORef env
  funs' <- case Map.lookup name env' of
    Nothing   -> pure []
    Just funs -> pure funs
  generalizedTypes <- pure $ generalizeTypes types
  writeIORef env (Map.insert name (if types == generalizedTypes then [(generalizedTypes, expr)] ++ funs' else funs' ++ [(generalizedTypes, expr)]) env')
  pure env

-- 与えられた名前と引数の型を持つ関数が存在するか？
funExists :: (Show a) => String -> RecList String -> GeneralEnv a -> IO Bool
funExists name types env = do
  fun <- lookupFun name types env True
  case fun of
    Nothing   -> pure $ False
    Just expr -> pure $ True


-- 与えられた名前と引数の型を持つ関数を探す
lookupFun :: (Show a) => String -> RecList String -> GeneralEnv a -> Bool -> IO (Maybe a)
lookupFun name types env strict = do
  --trace ("lookupFun: name=" ++ name) $ pure True
  env' <- readIORef env
  pure $ do
    funs <- Map.lookup name env'
    if hasVariable types
      then lastMatch (generalizeTypesWith "x" types) funs strict
      else firstMatch (generalizeTypesWith "x" types) funs strict

-- 環境を先頭から（具体型を持つほうから）探して、最初にマッチした関数を返す
firstMatch :: (Show a) => RecList String -> [(RecList String, a)] -> Bool -> Maybe a
firstMatch types [] strict = Nothing
firstMatch types ((types', expr):es) strict =
  if strict
    then if types' == types then Just expr else firstMatch types es strict
    else case unify (rInit types') types Map.empty of
      Nothing  -> firstMatch types es strict
      Just env -> Just expr

-- 環境を後ろから（抽象型を持つほうから）探して、最初にマッチした関数を返す
lastMatch :: (Show a) => RecList String -> [(RecList String, a)] -> Bool -> Maybe a
lastMatch types funs strict = firstMatch types (reverse funs) strict

-- 変数を環境に登録する
-- 同名の変数がない場合のみ登録できる
-- 同名の関数はあってもいい
insertVar :: (Show a) => String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
insertVar name expr env = do
  e <- varExists name env
  if not e
    then insertVarForce name expr env
    else pure $ Left ("variable '" ++ name ++ "' already exists")

-- 変数を環境に強制的に登録する
insertVarForce :: String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
insertVarForce name expr env = do
  env' <- readIORef env
  funs' <- case Map.lookup name env' of
    Nothing   -> pure []
    Just funs -> pure funs
  writeIORef env (Map.insert name ([(Elem "_", expr)] ++ funs') env')
  pure $ Right env

-- 与えられた名前を持つ変数が存在するか？
varExists :: (Show a) => String -> GeneralEnv a -> IO Bool
varExists name env = do
  exists <- lookupVar name env
  case exists of
    Nothing   -> pure False
    otherwise -> pure True

-- 与えられた名前の変数、または関数が存在するか？
anyExists :: String -> GeneralEnv a -> IO Bool
anyExists name env = do
  env' <- readIORef env
  case Map.lookup name env' of
    Nothing   -> pure False
    Just vars -> pure True

-- 環境の中身を表示する
showEnv :: (Show a) => GeneralEnv a -> IO ()
showEnv env = do
  env' <- readIORef env
  print env'
