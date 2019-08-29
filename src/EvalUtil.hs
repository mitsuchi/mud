-- Eval と TypeEval で共有するユーティリティ
module EvalUtil where

import           Data.IORef
import           Data.Map

import           Env
import           Expr
import           RecList

　-- 与えられた環境をベースに、関数の仮引数に対して実引数を登録した環境を作る
newEnv :: [String] -> [Expr] -> (Map String [(RecList String, Expr)]) -> IO Env
newEnv params args outerEnv = do
    env <- newIORef outerEnv
    mapM_ (\p -> insertAny p env) (zip params args)
    return env

-- 環境に変数または関数を登録する
insertAny :: (String, Expr) -> Env -> IO (Either String Env)
insertAny (name, expr) env = case expr of
    (Fun types _ _ _) -> insertFun name types expr env
    (TypeLit types)   -> case types of
        Elems types' -> insertFun name types expr env
        Elem type'   -> insertVarForce name expr env
    otherwise         -> insertVarForce name expr env

-- find のモナド版
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p []     = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)

-- if のモナド版
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

