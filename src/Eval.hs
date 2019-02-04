module Eval where

  import Control.Monad (forM_)
  import Control.Monad.Except
  import Data.IORef
  import Data.List (find)
  import Data.Map as Map hiding (map, foldr, take)
  import Debug.Trace

  import Expr
  import Env
  import DeepList
  import Primitive
  import Tuple
  import TypeUtil

  type IOThrowsError = ExceptT String IO

  eval :: Expr -> Env -> IOThrowsError Expr
  eval (IntLit i) env = return $ IntLit i
  eval (StrLit s) env = return $ StrLit s
  eval (DoubleLit f) env = return $ DoubleLit f
  eval (ListLit es) env = do
    es' <- mapM (\e -> eval e env) es
    return $ ListLit es'
  eval (Var "True" _) env = return $ BoolLit True
  eval (Var "False" _) env = return $ BoolLit False
  eval (Var name c) env = do
    var <- liftIO $ lookupVarLoose name env
    env' <- liftIO $ readIORef env
    case var of
      -- Nothing -> throwError ((show $ lineOfCode c) ++ ":variable '" ++ name ++ "' not found, env = " ++ (show env'))
      Nothing -> throwError ((show $ lineOfCode c) ++ ":variable '" ++ name ++ "' not found")
      Just x -> return x
  eval (Neg expr) env = eval (BinOp (OpLit "-") emptyCode (IntLit 0) expr) env
  eval (BinOp Eq _ (Var v _) e) env = do
    e' <- eval e env
    eval (Assign v e') env  
  eval (BinOp Dot _ e1 (Var v c)) env = eval (Apply (Var v c) [e1]) env
  eval (BinOp Dot _ e1 fun@(Fun types params expr outerEnv)) env = eval (Apply fun [e1]) env
  eval (BinOp Dot _ e1 (Apply expr args)) env = eval (Apply expr (e1 : args)) env
  eval (BinOp (OpLit lit) code e1 e2) env = eval (Apply (Var lit code) [e1, e2]) env  
  eval (BinOp op code e1 e2) env = do
    e1' <- eval e1 env
    e2' <- eval e2 env
    eval (BinOp op code e1' e2') env
  eval (Seq (e:[])) env = eval e env
  eval (Seq (e:es)) env = do
    eval e env
    eval (Seq es) env
  eval (Assign name fun@(Fun types params expr outerEnv)) env = do
    res <- liftIO $ insertFun name types fun env
    case res of
      Left message -> throwError message
      Right env -> return expr
  eval (Assign name expr) env = do
    res <- liftIO $ insertVar name expr env
    case res of
      Left message -> throwError message
      Right env -> return expr
  eval (FunDef name types params body) env = do
    eval (Assign name (Fun types params body env)) env  
  eval (FunDefAnon types params body) env = do
    return $ Fun types params body env
  eval (Apply (Call name) args) env = call name args env emptyCode
  eval (Apply (Fun types params body outerEnv) args) env = do
    varMap <- liftIO $ readIORef outerEnv
    env' <- liftIO $ newEnv params args varMap
    eval body env'
  eval (Apply (Var name code) args) env = do
    args' <- mapM (\arg -> eval arg env) args
    fun' <- liftIO $ lookupFun name (Plain (map typeOf' args')) env False
    case fun' of
      Just fun -> eval (Apply fun args') env
      Nothing -> call name args' env code
  eval (Apply expr args) env = do
    expr' <- eval expr env
    args' <- mapM (\arg -> eval arg env) args
    eval (Apply expr' args') env
  eval (Case es matchPairs types) env = do
    es' <- mapM (\e -> eval e env ) es
    pair' <- lift $ findM (\pair -> matchCond es' (fst3 pair) (thd3 pair) Map.empty env) matchPairs
    case pair' of 
      Just pair -> let (params, args) = paramsAndArgs (fst3 pair) es'
                   in eval (Apply (Fun types params (snd3 pair) env) args) env
      Nothing   -> throwError "condition no match"
  eval expr@(TypeSig sig (Var name _)) env = do
    fun' <- liftIO $ lookupFun name sig env False
    case fun' of
      Just fun -> return fun
      Nothing -> do
        env' <- liftIO $ readIORef env
        -- throwError (name ++ " not found , env = " ++ (show env'))
        throwError ("function '" ++ name ++ "' not found")
  eval (TypeSig sig expr) env = eval expr env
  eval (If condExpr thenExpr elseExpr) env = do
    cond' <- eval condExpr env
    case cond' of
      BoolLit True -> eval thenExpr env
      BoolLit False -> eval elseExpr env
      otherwise -> error ("cond = " ++ show (cond'))
  eval (TypeDef name typeDef) env = do
    forM_ typeDef $ \(member, (Plain [typeList])) -> do
      eval (FunDef member (Plain [Elem name, typeList]) ["x"] (Apply (Var "lookupStruct" emptyCode) [Var "x" emptyCode, StrLit member])) env
    eval (FunDef name types params (Apply (Var "makeStruct" emptyCode) (StrLit name : map StrLit params))) env    
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

  matchCond :: [Expr] -> [Expr] -> Maybe Expr -> (Map String Expr) -> Env -> IO Bool
  matchCond (IntLit i:e1s) (IntLit j:e2s) guard varMap env = if i == j then matchCond e1s e2s guard varMap env else return False
  matchCond (DoubleLit i:e1s) (DoubleLit j:e2s) guard varMap env = if i == j then matchCond e1s e2s guard varMap env else return False
  matchCond ((ListLit l1):e1s) ((ListLit [Var h _, Var t _]):e2s) guard varMap env = matchCond e1s e2s guard varMap env
  matchCond ((ListLit l1):e1s) ((ListLit l2):e2s) guard varMap env = if l1 == l2 then matchCond e1s e2s guard varMap env else return False
  matchCond (e0:e1s) ((Var v _):e2s) guard varMap env = case Map.lookup v varMap of
    Nothing -> matchCond e1s e2s guard (Map.insert v e0 varMap) env
    Just e  -> if e == e0
      then matchCond e1s e2s guard varMap env
      else return False
  matchCond [] [] guard varMap env = case guard of
    Nothing -> return True
    Just guard' -> do
      varMap' <- liftIO $ readIORef env
      env' <- newIORef varMap'
      mapM_ (\p -> insertAny p env') (toList varMap)
      bool <- runExceptT (eval guard' env')
      case bool of
        Right val -> return $ val == (BoolLit True)
        Left error -> trace error $ return False
  matchCond e1 e2 _ varMap env = trace ("matchCond: " ++ show (e1,e2)) $ return False

  findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
  findM p [] = return Nothing
  findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)

  ifM :: Monad m => m Bool -> m a -> m a -> m a
  ifM b t f = do b <- b; if b then t else f