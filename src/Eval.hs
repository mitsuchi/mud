module Eval where

  import Control.Monad (forM_)
  import Data.IORef
  import Data.List (find)
  import Data.Map as Map hiding (map, foldr, take)

  import Expr
  import Env
  import DeepList
  import Primitive
  import TypeUtil

  eval :: Expr -> Env -> IO Expr
  eval (IntLit i) env = return $ IntLit i
  eval (StrLit s) env = return $ StrLit s
  eval (DoubleLit f) env = return $ DoubleLit f
  eval (ListLit es) env = do
    es' <- mapM (\e -> eval e env) es
    return $ ListLit es'
  eval (Var "True") env = return $ BoolLit True
  eval (Var "False") env = return $ BoolLit False
  eval (Var name) env = do
    var <- lookupVarLoose name env
    env' <- readIORef env
    case var of
      Nothing -> error ("'" ++ name ++ "' not found, env = " ++ (show env'))
      Just x -> return x
  eval (Neg expr) env = eval (BinOp (OpLit "-") (IntLit 0) expr) env
  eval (BinOp Eq (Var v) e) env = do
    e' <- eval e env
    eval (Assign v e') env  
  eval (BinOp Dot e1 (Var v)) env = eval (Apply (Var v) [e1]) env
  eval (BinOp Dot e1 (Apply expr args)) env = eval (Apply expr (e1 : args)) env
  eval (BinOp (OpLit lit) e1 e2) env = eval (Apply (Var lit) [e1, e2]) env  
  eval (BinOp op e1 e2) env = do
    e1' <- eval e1 env
    e2' <- eval e2 env
    eval (BinOp op e1' e2') env
  eval (Seq (e:[])) env = eval e env
  eval (Seq (e:es)) env = do
    eval e env
    eval (Seq es) env
  eval (Assign name fun@(Fun types params expr outerEnv)) env = do
    res <- insertFun name types fun env
    case res of
      Left message -> error message
      Right env -> return expr
  eval (Assign name expr) env = do
    res <- insertVar name expr env
    case res of
      Left message -> error message
      Right env -> return expr
  eval (FunDef name types params body) env = do
    eval (Assign name (Fun types params body env)) env  
  eval (FunDefAnon types params body) env = do
    return $ Fun types params body env
  eval (Apply (Fun types params body outerEnv) args) env = do
    varMap <- readIORef outerEnv
    env' <- newEnv params args varMap
    eval body env'
  eval (Apply (Var name) args) env = do
    args' <- mapM (\arg -> eval arg env) args
    fun' <- lookupFun name (Plain (map typeOf' args')) env
    case fun' of
      Just fun -> eval (Apply fun args') env
      Nothing -> call name args' env
  eval (Apply expr args) env = do
    expr' <- eval expr env
    args' <- mapM (\arg -> eval arg env) args
    eval (Apply expr' args') env
  eval (Case es matchPairs types) env = do
    es' <- mapM (\e -> eval e env ) es
    case find (\pair -> matchCond es' (fst pair)) matchPairs of
      Just pair -> let (params, args) = paramsAndArgs (fst pair) es'
                   in eval (Apply (Fun types params (snd pair) env) args) env
      Nothing   -> error "condition no match"
  eval expr@(TypeSig sig (Var name)) env = do
    fun' <- lookupFun name sig env
    case fun' of
      Just fun -> return fun
      Nothing -> do
        env' <- readIORef env
        error (name ++ " not found , env = " ++ (show env'))
  eval (TypeSig sig expr) env = eval expr env
  eval (If condExpr thenExpr elseExpr) env = do
    cond' <- eval condExpr env
    case cond' of
      BoolLit True -> eval thenExpr env
      BoolLit False -> eval elseExpr env
  eval (TypeDef name typeDef) env = do
    forM_ typeDef $ \(member, (Plain [typeList])) -> do
      eval (FunDef member (Plain [Elem name, typeList]) ["x"] (Apply (Var "lookupStruct") [Var "x", StrLit member])) env
    eval (FunDef name types params (Apply (Var "makeStruct") (StrLit name : map StrLit params))) env    
    where types = typeDefToTypes typeDef
          params = map fst typeDef
  eval value@(StructValue structValue) env = return value

  newEnv :: [String] -> [Expr] -> (Map String [(DeepList String, Expr)]) -> IO Env
  newEnv params args outerEnv = do
    env <- newIORef outerEnv
    mapM_ (\p -> insertAny p env) (zip params args)
    return env

  insertAny :: (String, Expr) -> Env -> IO (Either String Env)
  insertAny (name, expr) env = case expr of
    (Fun types _ _ _) -> insertFun name types expr env
    otherwise         -> insertVarForce name expr env