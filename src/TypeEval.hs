module TypeEval where

  import Control.Monad (forM_)
  import Control.Monad.Except
  import Data.IORef
  import Data.List (find, intercalate)
  import Data.Map as Map hiding (map, foldr, take)
  import Debug.Trace

  import Expr
  import Env
  import DeepList
  import Primitive
  import Tuple
  import TypeUtil

  type IOThrowsError = ExceptT String IO

  typeEval :: Expr -> Env -> IOThrowsError (DeepList Type)
  typeEval (IntLit i) env = return $ Elem "Int"
  typeEval (StrLit s) env = return $ Elem "String"
  typeEval (BoolLit b) env = return $ Elem "Bool"
  typeEval (DoubleLit b) env = return $ Elem "Double"
  typeEval (Var "True" _) env = return $ Elem "Bool"
  typeEval (Var "False" _) env = return $ Elem "Bool"
  typeEval (TypeSig sig _) env = return $ sig
  typeEval (Fun sig _ _ _) env = return $ sig
  typeEval (ListLit [] _) env = return $ Plain [Elem "List", Elem "t0"]
  -- リストの型は、要素すべてと同じ型。ひとつでも違う型があるとエラー。
  typeEval (ListLit es c) env = do
    es' <- mapM (\e -> typeEval e env) es
    if allTheSame es'
      then return $ Plain [Elem "List", head es']
      else throwError $ (show $ lineOfCode c) ++ ":type mismatch. list can't contain different type of elements."
  typeEval (StructValue s) env = case Map.lookup "type" s of
    Just (StrLit str) -> return $ Elem str
    Nothing           -> error "type not defined in struct value"  
  typeEval (Seq (e:[])) env = typeEval e env
  -- 複式の型は最後の式の型なので、途中が違う型でもいい
  typeEval (Seq (e:es)) env = do
    typeEval e env
    typeEval (Seq es) env
  typeEval (Neg expr) env = typeEval (BinOp (OpLit "-") emptyCode (IntLit 0) expr) env
  typeEval (BinOp Eq _ v e) env = do
    t' <- typeEval e env
    typeEval (Assign v (TypeLit t')) env
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
      Nothing -> throwError ((show $ lineOfCode c) ++ ":variable '" ++ name ++ "' not found")
      Just (TypeLit x) -> return x
      Just (Fun types _ _ _) -> return types
      --otherwise -> throwError $ show var
  typeEval (BinOp (OpLit lit) code e1 e2) env = 
    typeEval (Apply (Var lit code) [e1, e2]) env 
  typeEval (BinOp op code e1 e2) env = do
    e1' <- typeEval e1 env
    e2' <- typeEval e2 env
    typeEval (BinOp op code (TypeLit e1') (TypeLit e2')) env
  typeEval (Apply (Var name code) args) env = do
    args' <- mapM (\arg -> typeEval arg env) args
    fun' <- liftIO $ lookupFun name (Plain args') env False
    case fun' of
      Just (Fun types _ _ _) -> return $ dLast types
      Just (Call name types) -> return $ dLast types
      Nothing -> throwError ((show $ lineOfCode code) ++ ":type mismatch. function '" ++ name ++ " : " ++ intercalate " -> " (map dArrow args') ++ " -> ?' not found.")
      otherwise -> throwError (show fun')
  typeEval (Apply (TypeLit types) args) env = do
    args' <- mapM (\arg -> typeEval arg env) args
    case findTypeEnv types (Plain args') Map.empty False of
      Nothing -> throwError $ "type mismatch. function has type : " ++ argSig types ++ ", but actual args are : " ++ intercalate " -> " (map dArrow args')
      Just env -> return $ dLast types
  typeEval (If condExpr thenExpr elseExpr) env = do
    (Elem cond') <- typeEval condExpr env
    (Elem then') <- typeEval thenExpr env
    (Elem else') <- typeEval elseExpr env
    if cond' == "Bool"
      then if then' == else'
        then return $ Elem then'
        else throwError $ "type mismatch. then-part has a type '" ++ then' ++ "', else-part has '" ++ else' ++ "'. they must be the same."
      else throwError $ "type mismatch. condition-part has a type '" ++ cond' ++ "'. must be 'Bool'."
  typeEval (FunDef nameExpr types params body) env = do
    -- 本体の型が返り値の型と一致する必要がある
    varMap <- liftIO $ readIORef env
    env' <- liftIO $ newEnv params (map TypeLit (dArgs types)) varMap
    body' <- typeEval body env'
    case findTypeEnv types (dAppend (dInit types) (Plain [body'])) Map.empty False of
      Just env0 -> typeEval (Assign nameExpr (Fun types params body env)) env 
      Nothing -> throwError $ "type mismatch. function supposed to return '" ++ dArrow (dLast types) ++ "', but actually returns '" ++ dArrow body' ++ "'"
  typeEval (FunDefAnon types params body) env = do
    return $ types
  typeEval (Apply expr args) env = do
    expr' <- typeEval expr env
    typeEval (Apply (TypeLit expr') args) env    
      
  allTheSame :: (Eq a) => [a] -> Bool
  allTheSame [] = True
  allTheSame (e:[]) = True
  allTheSame (e:es) = if e == head es
    then allTheSame es
    else False

  newEnv :: [String] -> [Expr] -> (Map String [(DeepList String, Expr)]) -> IO Env
  newEnv params args outerEnv = do
    env <- newIORef outerEnv
    mapM_ (\p -> insertAny p env) (zip params args)
    return env

  insertAny :: (String, Expr) -> Env -> IO (Either String Env)
  insertAny (name, expr) env = case expr of
    (Fun types _ _ _) -> insertFun name types expr env
    otherwise         -> insertVarForce name expr env