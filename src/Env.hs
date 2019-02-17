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
  -- 同名かつ同型の関数がない場合のみ登録できる
  insertFun :: (Show a) => String -> DeepList String -> a -> GeneralEnv a -> IO (Either String (GeneralEnv a))      
  insertFun name types expr env = do
    fe <- funExists name types env
    if not fe
    then do
      insertFun' name types expr env
      return $ Right env
    else
      return $ Left ("function '" ++ name ++ " : " ++ argSig types ++ "' already exists")

  -- 同名の関数がある場合は、具体型の関数は先頭に、多相型の関数は末尾に追加する      
  insertFun' :: String -> DeepList String -> a -> GeneralEnv a -> IO (GeneralEnv a)
  insertFun' name types expr env = do
    env' <- readIORef env
    funs' <- case Map.lookup name env' of
      Nothing -> return []
      Just funs -> return funs
    generalizedTypes <- return $ generalizeTypeSig types
    writeIORef env (Map.insert name (if types == generalizedTypes then [(generalizedTypes, expr)] ++ funs' else funs' ++ [(generalizedTypes, expr)]) env')
    return env

  funExists :: (Show a) => String -> DeepList String -> GeneralEnv a -> IO Bool
  funExists name types env = do
    fun <- lookupFun name types env True
    case fun of
      Nothing -> return $ False
      Just expr -> return $ True

  
  -- 
  lookupFun :: (Show a) => String -> DeepList String -> GeneralEnv a -> Bool -> IO (Maybe a)
  lookupFun name types env strict = do
    --trace ("lookupFun: name=" ++ name) $ return True
    env' <- readIORef env
    return $ do
      funs <- Map.lookup name env'
      if hasVariable types
        then lastMatch (generalizeTypesWith "x" types) funs strict
        --else trace ("firstMatch: type: " ++ (show (generalizeTypesWith "x" types)) ++ ", funs: " ++ (show funs) ++ ", strict: " ++ (show strict)) $ firstMatch (generalizeTypesWith "x" types) funs strict
        else firstMatch (generalizeTypesWith "x" types) funs strict
      -- if hasVariable types
      --   then let fun' = firstMatch (generalizeTypesWith "x" types) funs strict True
      --     in case fun' of
      --       Nothing -> firstMatch (generalizeTypesWith "x" types) funs strict False
      --       Just fun -> fun'
      --   else firstMatch (generalizeTypesWith "x" types) funs strict False

  firstMatch :: (Show a) => DeepList String -> [(DeepList String, a)] -> Bool -> Maybe a
  firstMatch types [] strict = Nothing
  firstMatch types ((types', expr):es) strict = 
    case (if strict then findTypeEnv types' types Map.empty True else unify (dInit types') types Map.empty) of
    Nothing -> firstMatch types es strict
    Just env -> Just expr

  lastMatch :: (Show a) => DeepList String -> [(DeepList String, a)] -> Bool -> Maybe a
  lastMatch types funs strict = firstMatch types (reverse funs) strict

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

