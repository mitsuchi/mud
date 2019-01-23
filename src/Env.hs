module Env where

    import Data.Char
    import Data.IORef
    import Data.Map as Map hiding (map, foldr)
    import DeepList
    import TypeUtil

    type Type = String
    type Name = String
    type Param = String
    type Env = IORef (Map Name [(DeepList Type, Expr)])

    data Expr
      = IntLit Integer
      | StrLit String
      | Var Name
      | BinOp Op Expr Expr
      | Seq [Expr]
      | Assign Name Expr
      | FunDef Name (DeepList Type) [Param] Expr
      | Fun (DeepList Type) [Param] Expr Env
      | Apply Expr [Expr]
      | Case [Expr] [([Expr],Expr)] (DeepList Type)
      | TypeSig (DeepList Type) Expr

    data Op
      = Mul
      | Div
      | Add
      | Sub
      | Eq
      | Dot
      | RArrow
      | Colon
      deriving (Show)

    instance Show Expr where
      show (IntLit i1) = show i1  
      show (StrLit str) = str
      show (Var name) = "Var " ++ name
      show (BinOp op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
      show (Seq exprs) = foldr ((++).(++ ";").show) "" exprs  
      show (Fun types params body env) = "function : " ++ (show types)
      show (FunDef name types params body) = name
      show (Apply e1 e2) = "application : " ++ show (e1) ++ " on " ++ show (e2)
      show (TypeSig sig expr) = (show expr) ++ " : " ++ (show sig)

    -- 変数を環境に登録する
    -- 同名の変数も関数もない場合のみ登録できる
    insertVar :: Name -> Expr -> Env -> IO (Either String Env)
    insertVar name expr env = do
      e <- anyExists name env
      if not e
      then do
        env' <- readIORef env
        writeIORef env (Map.insert name [(Elem "_", expr)] env')
        return $ Right env
      else return $ Left (name ++ " already exists")
    
    varExists :: Name -> Env -> IO Bool
    varExists name env = do
      exists <- lookupVar name env
      case exists of
        Nothing -> return False
        otherwise -> return True
    
    lookupVar :: Name -> Env -> IO (Maybe Expr)
    lookupVar name env = do
      env' <- readIORef env
      case Map.lookup name env' of
        Nothing -> return Nothing
        Just vars -> case vars of
          (Elem "_", expr):[] -> return $ Just expr
          otherwise -> return Nothing

    anyExists :: Name -> Env -> IO Bool
    anyExists name env = do
      env' <- readIORef env
      case Map.lookup name env' of
        Nothing -> return False
        Just vars -> return True

    showEnv :: Env -> IO ()
    showEnv env = do
      env' <- readIORef env
      print env'

    showResult :: Either String Env -> IO ()
    showResult res = case res of
      Left message -> print message
      Right env -> showEnv env

    -- 関数を環境に登録する
    -- 同名の変数も、同型の関数もない場合のみ登録できる
    -- ただし、同名の関数がある場合はその定義の末尾に追加する
    insertFun :: Name -> DeepList Type -> Expr -> Env -> IO (Either String Env)      
    insertFun name types expr env = do
      ve <- varExists name env
      fe <- funExists name types env
      if not ve && not fe
      then do
        insertFun' name (generalizeTypeSig types) expr env
        return $ Right env
      else
        return $ Left (name ++ " already exists"  )

    insertFun' :: Name -> DeepList Type -> Expr -> Env -> IO Env
    insertFun' name types expr env = do
      env' <- readIORef env
      funs' <- case Map.lookup name env' of
        Nothing -> return []
        Just funs -> return funs
      writeIORef env (Map.insert name (funs' ++ [(generalizeTypeSig types, expr)]) env')
      return env

    generalizeTypeSig :: DeepList String -> DeepList String
    generalizeTypeSig list = gnrlize' list (makeMap (dFlatten list))
  
    gnrlize' :: DeepList String -> Map String Int -> DeepList String
    gnrlize' (Elem e) table = case Map.lookup e table of
      Nothing -> Elem e
      Just i -> Elem ("t" ++ show i)
    gnrlize' (Plain []) table = Plain []
    gnrlize' (Plain (e:es)) table = 
      let (Plain rest') = (gnrlize' (Plain es) table)
      in Plain ((gnrlize' e table) : rest')

    makeMap :: [String] -> Map String Int
    makeMap list = makeMap' list 0 Map.empty

    makeMap' :: [String] -> Int -> Map String Int -> Map String Int
    makeMap' [] num table = table
    makeMap' (e:es) num table = if isUpper(e !! 0) then (makeMap' es num table) else 
      case Map.lookup e table of
        Nothing -> makeMap' es (num+1) (Map.insert e num table)
        Just i  -> makeMap' es num table

    funExists :: Name -> DeepList Type -> Env -> IO Bool
    funExists name types env = do
      fun <- lookupFun name types env
      case fun of
        Nothing -> return $ False
        Just expr -> return $ True

    lookupFun :: Name -> DeepList Type -> Env -> IO (Maybe Expr)
    lookupFun name types env = do
      env' <- readIORef env
      case Map.lookup name env' of
        Nothing -> return Nothing
        Just funs -> case funs of
          (Elem "_", expr):[] -> return Nothing
          otherwise -> case firstMatch (generalizeTypeSig types) funs of
            Nothing -> return Nothing
            Just fun -> return $ Just fun

    firstMatch :: DeepList Type -> [(DeepList Type, Expr)] -> Maybe Expr
    firstMatch types [] = Nothing
    firstMatch types ((types', expr):es) = case findTypeEnv types' types Map.empty of
      Nothing -> firstMatch types es
      Just env -> Just expr