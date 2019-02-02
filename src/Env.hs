module Env where

  import Data.IORef
  import Data.Map as Map
  import Debug.Trace
  import DeepList
  import TypeUtil

  type GeneralEnv a = IORef (Map String [(DeepList String, a)])

  lookupVar :: (Show a) => String -> GeneralEnv a -> IO (Maybe a)
  lookupVar name env = lookupVar' name env False

  lookupVarLoose :: (Show a) => String -> GeneralEnv a -> IO (Maybe a)
  lookupVarLoose name env = lookupVar' name env True

  lookupVar' :: (Show a) => String -> GeneralEnv a -> Bool -> IO (Maybe a)
  lookupVar' name env loose = do
    env' <- readIORef env
    return $ do
      vars <- Map.lookup name env'
      case vars of
        -- 同じ名前の定義の先頭に変数があればそれを参照する
        (Elem "_", expr):es -> Just expr
        -- 変数として名前がなくても関数として1つだけ名前があるならそれを参照する
        (Plain _, expr):[] -> if loose then Just expr else Nothing
        -- そうでなければなし
        otherwise          -> Nothing
        
  -- 関数を環境に登録する
  -- 同名の変数も、同型の関数もない場合のみ登録できる
  -- ただし、同名の関数がある場合はその定義の末尾に追加する
  insertFun :: (Show a) => String -> DeepList String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))      
  insertFun name types expr env = do
    -- ve <- varExists name env
    fe <- funExists name types env
    --if not ve && not fe
    if not fe
    then do
      insertFun' name (generalizeTypeSig types) expr env
      return $ Right env
    else
      return $ Left ("function '" ++ name ++ "' already exists"  )

  insertFun' :: String -> DeepList String -> a -> GeneralEnv a -> IO (GeneralEnv a)
  insertFun' name types expr env = do
    env' <- readIORef env
    funs' <- case Map.lookup name env' of
      Nothing -> return []
      Just funs -> return funs
    writeIORef env (Map.insert name (funs' ++ [(generalizeTypeSig types, expr)]) env')
    return env

  funExists :: (Show a) => String -> DeepList String -> GeneralEnv a -> IO Bool
  funExists name types env = do
    fun <- lookupFun name types env
    case fun of
      Nothing -> return $ False
      Just expr -> return $ True

  lookupFun :: (Show a) => String -> DeepList String -> GeneralEnv a -> IO (Maybe a)
  lookupFun name types env = do
    env' <- readIORef env
    return $ do
      funs <- Map.lookup name env'
      firstMatch (generalizeTypeSig types) funs

  firstMatch :: DeepList String -> [(DeepList String, a)] -> Maybe a
  firstMatch types [] = Nothing
  firstMatch types ((types', expr):es) = case findTypeEnv types' types Map.empty of
    Nothing -> firstMatch types es
    Just env -> Just expr    

  -- 変数を環境に登録する
  -- 同名の変数がない場合のみ登録できる
  -- 同名の関数はあってもいい
  insertVar :: (Show a) => String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
  insertVar name expr env = do
    e <- varExists name env
    if not e
      then insertVarForce name expr env
      else return $ Left ("variable '" ++ name ++ "' already exists")

  insertVarForce :: String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))
  insertVarForce name expr env = do
    env' <- readIORef env
    funs' <- case Map.lookup name env' of
      Nothing -> return []
      Just funs -> return funs
    writeIORef env (Map.insert name ([(Elem "_", expr)] ++ funs') env')
    return $ Right env

  varExists :: (Show a) => String -> GeneralEnv a -> IO Bool
  varExists name env = do
    exists <- lookupVar name env
    case exists of
      Nothing -> return False
      otherwise -> return True

  anyExists :: String -> GeneralEnv a -> IO Bool
  anyExists name env = do
    env' <- readIORef env
    case Map.lookup name env' of
      Nothing -> return False
      Just vars -> return True

  showEnv :: (Show a) => GeneralEnv a -> IO ()
  showEnv env = do
    env' <- readIORef env
    print env'

