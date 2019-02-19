module TypeEval where

  import Control.Monad (forM_)
  import Control.Monad.Except
  import Data.Char
  import Data.IORef
  import Data.List (find, intercalate)
  import qualified Data.Map as Map hiding (foldr, take)
  import Debug.Trace

  import Expr
  import Env
  import RecList
  import Primitive
  import Tuple
  import TypeUtil 

  typeEval :: Expr -> Env -> IOThrowsError (RecList Type)
  typeEval (IntLit i) env = return $ Elem "Int"
  typeEval (StrLit s) env = return $ Elem "String"
  typeEval (BoolLit b) env = return $ Elem "Bool"
  typeEval (DoubleLit b) env = return $ Elem "Double"
  typeEval (Var "True" _) env = return $ Elem "Bool"
  typeEval (Var "False" _) env = return $ Elem "Bool"
  typeEval (TypeSig sig _) env = return $ sig
  typeEval (TypeLit types) env = return types
  typeEval (Fun sig _ _ _) env = return $ sig
  typeEval (ListLit [] _) env = return $ Elems [Elem "List", Elem "t0"]
  -- リストの型は、要素すべてと同じ型。ひとつでも違う型があるとエラー。
  typeEval (ListLit es c) env = do
    es' <- mapM (\e -> typeEval e env) es
    if allTheSame es'
      then return $ Elems [Elem "List", head es']
      else throwError $ (show $ lineOfCode c) ++ ":type mismatch. list can't contain different type of elements."
  typeEval (StructValue s) env = case Map.lookup "type" s of
    Just (StrLit str) -> return $ Elem str
    Nothing           -> error "type not defined in struct value"  
  typeEval (Seq (e:[])) env = typeEval e env
  -- 複式の型は最後の式の型なので、途中が違う型でもいい
  typeEval (Seq (e:es)) env = do
    typeEval e env
    typeEval (Seq es) env
  typeEval (Neg expr) env = typeEval expr env
  typeEval (Assign (Var name code) fun@(Fun types params expr outerEnv)) env = do
    res <- liftIO $ insertFun name types fun env
    case res of
      Left message -> throwError $ (show $ lineOfCode code) ++ ":" ++ message
      Right env -> return types
  typeEval (Assign (Var name code) (TypeLit type')) env = do
    res <- liftIO $ insertVar name (TypeLit type') env
    case res of
      Left message -> throwError $ (show $ lineOfCode code) ++ ":" ++ message
      Right env -> return type'
  typeEval (Var name c) env = do
    var <- liftIO $ lookupVarLoose name env
    env' <- liftIO $ readIORef env
    case var of
      Nothing -> throwError ((show $ lineOfCode c) ++ ":variable '" ++ name ++ "' not found.")
      Just (TypeLit x) -> return x
      Just (Fun types _ _ _) -> return types
      otherwise -> throwError $ show var
  typeEval (BinOp Eq _ v e) env = do
    t' <- typeEval e env
    typeEval (Assign v (TypeLit t')) env
  typeEval (BinOp Dot _ e1 var@(Var name code)) env = typeEval (Apply var [e1]) env
  typeEval (BinOp Dot _ e1 fun@(Fun types params expr outerEnv)) env = typeEval (Apply fun [e1]) env
  typeEval (BinOp Dot _ e1 (Apply expr args)) env = typeEval (Apply expr (e1 : args)) env  
  typeEval (BinOp Dot _ e1 e2) env = typeEval (Apply e2 [e1]) env  
  typeEval (BinOp (OpLit lit) code e1 e2) env = 
    typeEval (Apply (Var lit code) [e1, e2]) env 
  typeEval (BinOp op code e1 e2) env = do
    e1' <- typeEval e1 env
    e2' <- typeEval e2 env
    typeEval (BinOp op code (TypeLit e1') (TypeLit e2')) env
  typeEval (Apply (Var name code) args) env = do
    args' <- mapM (\arg -> typeEval arg env) args
    fun' <- liftIO $ lookupFun name (Elems args') env False
    --trace ("args': " ++ (show args') ++ ", fun " ++ (show fun')) $ return ()
    types <- case fun' of
      Just (Fun types _ _ _) -> return types
      Just (Call name types) -> return types
      Just (TypeLit types) -> return types
      Nothing -> throwError ((show $ lineOfCode code) ++ ":type mismatch. function '" ++ name ++ " : " ++ intercalate " -> " (map rArrow args') ++ " -> ?' not found")
      otherwise -> throwError ("function not found. fun " ++ (show fun'))
    ts <- return $ generalizeTypesWith "t" types
    xs <- return $ generalizeTypesWith "x" (Elems args')
    case unify (rInit ts) xs Map.empty of
      Nothing -> throwError ((show $ lineOfCode code) ++ "ts:" ++ (show ts) ++ ",xs:" ++ (show xs) ++ ":fugafuga type mismatch. function '" ++ name ++ " : " ++ intercalate " -> " (map rArrow args') ++ " -> ?' not found.")
      --Just typeEnv -> trace ("unify ts: " ++ (show ts) ++ ", xs: " ++ (show xs) ++ ", args: " ++ (show args')) $ return $ typeInst (rLast ts) (Map.map (\e -> typeInst e typeEnv) typeEnv)
      Just typeEnv -> let env1 = Map.map (\e -> typeInst e typeEnv) typeEnv
        in return $ typeInst (rLast ts) $ Map.mapWithKey (\k e -> cancelXs k e env1) env1
    -- case typeEnv of
    --   Just tenv -> return $ typeInst (rLast types) tenv
    --   Nothing -> throwError ((show $ lineOfCode code) ++ ":type mismatch. can not apply function")
  typeEval (Apply (TypeLit types) args) env = do
    args' <- mapM (\arg -> typeEval arg env) args
    ts <- return $ generalizeTypesWith "t" types
    xs <- return $ generalizeTypesWith "x" (Elems args')
    case unify (rInit ts) xs Map.empty of
      Nothing -> throwError $ "type mismatch. function has type : " ++ argSig types ++ ", but actual args are : " ++ intercalate " -> " (map rArrow args')
      --Just typeEnv -> return $ rLast types
      Just typeEnv -> let env1 = Map.map (\e -> typeInst e typeEnv) typeEnv
        in return $ typeInst (rLast ts) $ Map.mapWithKey (\k e -> cancelXs k e env1) env1
  typeEval (Apply expr args) env = do
    expr' <- typeEval expr env
    typeEval (Apply (TypeLit expr') args) env        
  typeEval (If condExpr thenExpr elseExpr) env = do
    cond' <- typeEval condExpr env
    then' <- typeEval thenExpr env
    else' <- typeEval elseExpr env
    if cond' == Elem "Bool"
      then if then' == else'
        then return then'
        --else throwError $ "type mismatch. then-part has a type '" ++ (show then') ++ "', else-part has '" ++ (show else') ++ "'. they must be the same."
        else error $ "type mismatch. then-part has a type '" ++ (show then') ++ "', else-part has '" ++ (show else') ++ "'. they must be the same."
      else throwError $ "type mismatch. condition-part has a type '" ++ (show cond') ++ "'. must be 'Bool'."
  typeEval (FunDef nameExpr@(Var name code) types params body) env = do
    -- 本体の型が返り値の型と一致する必要がある
    varMap <- liftIO $ readIORef env
    env' <- liftIO $ newEnv params (map TypeLit (rArgs (generalizeTypes types))) varMap    
    -- 関数が再帰的に定義される可能性があるので、いま定義しようとしてる関数を先に型環境に登録しちゃう
    res <- liftIO $ insertFun name types (Fun types params body env) env'
    body' <- typeEval body env'
    case unify types (rAppend (rInit types) (Elems [body'])) Map.empty of
      Just env0 -> typeEval (Assign nameExpr (Fun types params body env)) env 
      Nothing -> throwError $ "type mismatch. function supposed to return '" ++ rArrow (rLast types) ++ "', but actually returns '" ++ rArrow body' ++ "'"
  typeEval (FunDefAnon types params body) env = do
    -- 本体の型が返り値の型と一致する必要がある
    varMap <- liftIO $ readIORef env
    env' <- liftIO $ newEnv params (map TypeLit (rArgs (generalizeTypes types))) varMap
    varMap' <- liftIO $ readIORef env'
    body' <- typeEval body env'
    case unify types (rAppend (rInit types) (Elems [body'])) Map.empty of      
      --Just env0 -> return $ types
      Just env0 -> return $ generalizeTypes types
      Nothing -> throwError $ "type mismatch. function supposed to return '" ++ rArrow (rLast types) ++ "', but actually returns '" ++ rArrow body' ++ "'"    
  typeEval (Case es matchPairs (Elems types')) env = do
    (Elems types) <- return $ generalizeTypes (Elems types')
    matchAll <- liftIO $ andM $ map ( \(args, body, guard) -> do
      (bool, typeEnv) <- matchCondType (init types) args guard Map.empty env
      bool' <- if bool then matchResultType body (last types) typeEnv env else return False
      return bool'
      ) matchPairs
    if matchAll
      then return $ last types
      else throwError "type mismatch. condition no match"
  typeEval (TypeDef (Var name code) typeDef) env = do
    forM_ typeDef $ \(member, (Elems [typeList])) -> do
      typeEval (FunDef (Var member code) (Elems [Elem name, typeList]) ["x"] (TypeLit typeList)) env
    typeEval (FunDef (Var name code) types params (TypeLit (rLast types))) env    
    where types = typeDefToTypes typeDef name
          params = map fst typeDef

  typeEvalWithPrimitiveEnv :: Expr -> IOThrowsError Expr
  typeEvalWithPrimitiveEnv expr = do
    env <- liftIO $ newIORef Map.empty
    liftIO $ insertPrimitives env
    expr' <- typeEval expr env
    return $ TypeLit expr'

  typeEvalWithEnv :: Expr -> Env -> IOThrowsError Expr
  typeEvalWithEnv expr env = do
    varMap <- liftIO $ readIORef env
    env' <- liftIO $ newIORef (Map.map ( \xs -> Prelude.map (\(types, expr) -> (types, (TypeLit . typeOf') expr)) xs ) varMap)
    liftIO $ insertPrimitives env'
    expr' <- typeEval expr env'
    return $ TypeLit expr'   

  -- body types typeEnv env
  -- 型環境 typeEnv と env のもとで body を評価して、その型が types とマッチするかどうか
  matchResultType :: Expr -> RecList Type -> Map.Map String (RecList Type) -> Env -> IO Bool
  matchResultType body types typeEnv env = do
    varMap' <- liftIO $ readIORef env
    env' <- newIORef varMap'
    mapM_ (\(name, types) -> insertAny (name, TypeLit types) env') (Map.toList typeEnv)
    bodyType <- runExceptT $ typeEval body env'
    --trace ("bodyType: " ++ (show bodyType) ++ ", types: " ++ (show types)) $ return True
    case bodyType of
      Left error -> return False
      Right bodyType' -> case unify types bodyType' typeEnv of
        Just env0 -> return True
        Nothing -> return False

  allTheSame :: (Eq a) => [a] -> Bool
  allTheSame [] = True
  allTheSame (e:[]) = True
  allTheSame (e:es) = if e == head es
    then allTheSame es
    else False

  newEnv :: [String] -> [Expr] -> (Map.Map String [(RecList String, Expr)]) -> IO Env
  newEnv params args outerEnv = do
    env <- newIORef outerEnv
    mapM_ (\p -> insertAny p env) (zip params args)
    return env

  insertAny :: (String, Expr) -> Env -> IO (Either String Env)
  insertAny (name, expr) env = case expr of
    (Fun types _ _ _) -> insertFun name types expr env
    (TypeLit types)   -> case types of
      Elems types' -> insertFun name types expr env
      Elem type'   -> insertVarForce name expr env
    otherwise         -> insertVarForce name expr env

  -- マッチ式の引数の列が、与えられた型の列(マッチ式を含む関数の型)とマッチするか？
  matchCondType :: [RecList Type] -> [Expr] -> Maybe Expr -> Map.Map String (RecList Type) -> Env -> IO (Bool, Map.Map String (RecList Type))
  matchCondType (e1:e1s) ((Var v _):e2s) guard varMap env = 
    -- マッチ式に変数がくる場合：変数に対する型の既存の割り当てと矛盾しなければマッチ      
    case Map.lookup v varMap of
      Nothing   -> matchCondType e1s e2s guard (Map.insert v e1 varMap) env
      Just types -> if types == e1
        then matchCondType e1s e2s guard varMap env
        else return (False, Map.empty)
  matchCondType (listType@(Elems [Elem "List", Elem a]):e1s) ((ListLit [Var e _, Var es _] _):e2s) guard varMap env = do
    -- マッチ式にリスト([e;es])がくる場合
    -- 対応する引数の型もリストであることが必要
    -- それを [a] とすると、e:a, es:[a] を型環境に割り当てる
    vmap1 <- return $ Map.insert e (Elem a) varMap
    vmap2 <- return $ Map.insert es listType vmap1
    matchCondType e1s e2s guard vmap2 env
  matchCondType (e1:e1s) (e2:e2s) guard varMap env =
    -- 一般の場合：型としてマッチすればOK (Int vs Int, a vs String など)    
    case unify e1 (typeOf' e2) varMap of
      Just varMap' -> matchCondType e1s e2s guard varMap' env
      Nothing -> return (False, Map.empty)
  matchCondType [] [] guard varMap env = case guard of
    Nothing -> return (True, varMap)
    Just guard' -> do
      -- もしガード節があれば、現状の型環境の下でガード節の型がBoolになることが必要
      varMap' <- liftIO $ readIORef env
      env' <- newIORef varMap'
      mapM_ (\(name, types) -> insertAny (name, TypeLit types) env') (Map.toList varMap)
      guardBodyType <- runExceptT (typeEval guard' env')
      case guardBodyType of
        Right val -> if val == (Elem "Bool")
          then return (True, varMap)
          else return (False, Map.empty)
        Left error -> trace error $ return (False, Map.empty)
  matchCondType e1 e2 _ varMap env = trace ("matchCondType: " ++ show (e1,e2)) $ return (False, Map.empty)

  allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
  allM p [] = return True
  allM p (x:xs) = ifM (p x) (allM p xs) (return False)

  andM :: Monad m => [m Bool] -> m Bool
  andM = allM id

  ifM :: Monad m => m Bool -> m a -> m a -> m a
  ifM b t f = do b <- b; if b then t else f

  -- 型もしくは型変数を、型環境を元にインスタンス化する
  typeInst :: RecList Type -> Map.Map String (RecList Type) -> RecList Type
  typeInst (Elem a) env | isUpper(a!!0) = Elem a
  typeInst (Elem a) env | isLower(a!!0) = case Map.lookup a env of
    Just types -> types
    Nothing -> Elem a
  typeInst (Elems es) env = Elems (map (\e -> typeInst e env) es)

  -- 型環境において、キーとなる型変数の値が型変数だった場合、値をキー自身で上書きする
  cancelXs :: String -> RecList Type -> Map.Map String (RecList Type) -> RecList Type
  cancelXs key value env | isConcrete value = value
  --cancelXs key value env | hasVariable value = Elem key
  cancelXs key value env | hasVariable value = renameType value

  -- [x0] -> x1 を [t0] -> t1 に変更する
  renameType :: RecList Type -> RecList Type
  renameType (Elem ('x':xs)) = Elem ('t':xs)
  renameType (Elem xs) = Elem xs
  renameType (Elems es) = Elems (map renameType es)


