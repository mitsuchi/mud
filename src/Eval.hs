-- 式の評価
module Eval where

import           Control.Monad        (forM_)
import           Control.Monad.Except
import           Data.IORef
import           Data.List            (find)
import           Data.Map             as Map hiding (foldr, map, take)
import           Debug.Trace

import           Env
import           EvalUtil
import           Expr
import           Primitive
import           RecList
import           TypeUtil

-- 与えられた環境の元で式を評価する
eval :: Expr -> Env -> IOThrowsError Expr
eval (IntLit i) env = pure $ IntLit i
eval (StrLit s) env = pure $ StrLit s
eval (DoubleLit f) env = pure $ DoubleLit f
eval (ListLit es c) env = do
  es' <- mapM (`eval` env) es
  pure $ ListLit es' c
eval (Var "True" _) env = pure $ BoolLit True
eval (Var "False" _) env = pure $ BoolLit False
eval (Var name c) env = do
  var <- liftIO $ lookupVarLoose name env
  env' <- liftIO $ readIORef env
  case var of
    Nothing -> throwError (show (lineOfCode c) ++ ":variable '" ++ name ++ "' not found")
    Just x -> pure x
eval (Neg expr) env = eval (BinOp (OpLit "-") emptyCode (IntLit 0) expr) env
eval (BinOp Eq _ v e) env = do
  e' <- eval e env
  eval (Assign v e') env
eval (BinOp Dot _ e1 var@(Var name code)) env = eval (Apply var [e1]) env
eval (BinOp Dot _ e1 fun@(Fun types params expr outerEnv)) env = eval (Apply fun [e1]) env
eval (BinOp Dot _ e1 (Apply expr args)) env = eval (Apply expr (e1 : args)) env
eval (BinOp (OpLit lit) code e1 e2) env = eval (Apply (Var lit code) [e1, e2]) env
eval (BinOp op code e1 e2) env = do
  e1' <- eval e1 env
  e2' <- eval e2 env
  eval (BinOp op code e1' e2') env
eval (Seq [e]) env = eval e env
eval (Seq (e:es)) env = do
  eval e env
  eval (Seq es) env
eval (Assign (Var name code) fun@(Fun types params expr outerEnv)) env = do
  res <- liftIO $ insertFun name types fun env
  case res of
    Left message -> throwError $ show (lineOfCode code) ++ ":" ++ message
    Right env    -> pure expr
eval (Assign (Var name code) expr) env = do
  res <- liftIO $ insertVar name expr env
  case res of
    Left message -> throwError $ show (lineOfCode code) ++ ":" ++ message
    Right env    -> pure expr
eval (FunDef nameExpr types params body) env =
  eval (Assign nameExpr (Fun types params body env)) env
eval (FunDefAnon types params body code) env =
  pure $ Fun types params body env
eval (Apply (Call name _) args) env = call name args env emptyCode
eval (Apply (Fun types params body outerEnv) args) env = do
  varMap <- liftIO $ readIORef outerEnv
  env'   <- liftIO $ newEnv params args varMap
  eval body env'
eval (Apply (Var name code) args) env = do
  args' <- mapM (`eval` env) args
  fun'  <- liftIO $ lookupFun name (Elems (map typeOf' args')) env False
  case fun' of
    Just fun -> eval (Apply fun args') env
    Nothing  -> call name args' env code
eval (Apply expr args) env = do
  expr' <- eval expr env
  args' <- mapM (`eval` env) args
  eval (Apply expr' args') env
eval (Case es matchExprs types) env = do
  es' <- mapM (`eval` env) es
  pair' <- lift $ findM (\(args, body, guard, code) -> matchCond es' args guard mempty env) matchExprs
  case pair' of
    Just (args, body, guard, code) -> let (params, args') = paramsAndArgs args es'
                 in eval (Apply (Fun types params body env) args') env
    Nothing   -> throwError "condition no match"
eval expr@(TypeSig sig (Var name _)) env = do
  fun' <- liftIO $ lookupFun name (rInit sig) env False
  case fun' of
    Just fun -> pure fun
    Nothing -> do
      env' <- liftIO $ readIORef env
      throwError ("function '" ++ name ++ "' not found")
eval (TypeSig sig expr) env = eval expr env
eval (If condExpr thenExpr elseExpr code) env = do
  cond' <- eval condExpr env
  case cond' of
    BoolLit True  -> eval thenExpr env
    BoolLit False -> eval elseExpr env
    _             -> error ("cond = " ++ show cond')
eval (TypeDef (Var name code) typeDef) env = do
  forM_ typeDef $ \(member, Elems [typeList]) ->
    eval (FunDef (Var member code) (Elems [Elem name, typeList]) ["x"] (Apply (Var "lookupStruct" emptyCode) [Var "x" emptyCode, StrLit member])) env
  eval (FunDef (Var name code) types params (Apply (Var "makeStruct" emptyCode) (StrLit name : map StrLit params))) env
  where types = typeDefToTypes typeDef name
        params = map fst typeDef
eval value@(StructValue structValue) env = pure value

-- プリミティブな関数だけを登録した環境で式を評価する
evalWithPrimitiveEnv :: Expr -> IOThrowsError Expr
evalWithPrimitiveEnv expr = do
  env <- liftIO $ newIORef mempty
  liftIO $ insertPrimitives env
  eval expr env

-- パターンマッチが成功するか？
matchCond :: [Expr] -> [Expr] -> Maybe Expr -> Map String Expr -> Env -> IO Bool
matchCond (IntLit i:e1s) (IntLit j:e2s) guard varMap env =
  if i == j then matchCond e1s e2s guard varMap env else pure False
matchCond (DoubleLit i:e1s) (DoubleLit j:e2s) guard varMap env =
  if i == j then matchCond e1s e2s guard varMap env else pure False
matchCond (StrLit i:e1s) (StrLit j:e2s) guard varMap env =
  if i == j then matchCond e1s e2s guard varMap env else pure False
matchCond (ListLit l1 _:e1s) (ListLit [Var h _, Var t _] _:e2s) guard varMap env =
  matchCond e1s e2s guard varMap env
matchCond (ListLit l1 _:e1s) (ListLit l2 _:e2s) guard varMap env =
  if l1 == l2 then matchCond e1s e2s guard varMap env else pure False
matchCond (e0:e1s) (Var v _:e2s) guard varMap env =
  case varMap !? v of
    Nothing -> matchCond e1s e2s guard (Map.insert v e0 varMap) env
    Just e  -> if e == e0
      then matchCond e1s e2s guard varMap env
      else pure False
matchCond [] [] guard varMap env =
  case guard of
    Nothing -> pure True
    Just guard' -> do
      varMap' <- liftIO $ readIORef env
      env' <- newIORef varMap'
      mapM_ (`insertAny` env') (toList varMap)
      bool <- runExceptT (eval guard' env')
      case bool of
        Right val  -> pure $ val == BoolLit True
        Left error -> trace error $ pure False
matchCond e1 e2 _ varMap env =
  trace ("matchCond: " ++ show (e1,e2)) $ pure False
