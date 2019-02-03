module Primitive where

  import Control.Monad.Except
  import Data.List
  import Data.Map as Map hiding (map)
  import Data.IORef

  import DeepList
  import Env
  import Expr
  
  call :: Name -> [Expr] -> Env -> Code -> ExceptT String IO Expr
  call "head" [ListLit (e:es)] env _ = return e
  call "tail" [ListLit (e:es)] env _ = return $ ListLit es
  call "puts" [e] env _ = do
    lift $ putStrLn (show e)
    return e
  call "makeStruct" (name:args) env _ = do
    lift $ makeStruct args (Map.fromList [("type", name)]) env
  call "lookupStruct" [StructValue structValue, StrLit member] env _ = do
    case Map.lookup member structValue of
      Just expr -> return expr
      Nothing -> throwError ("can't find struct member '" ++ member ++ "'")
  call "to_s" [IntLit i] env _ = return $ StrLit (show i)
  call "to_s" [DoubleLit d] env _ = return $ StrLit (show d)
  call "to_s" [ListLit l] env _ = return $ StrLit (show l)
  call "to_s" [BoolLit b] env _ = return $ StrLit (show b)
  call "debug" [StrLit s] env _ = do
    env' <- liftIO $ readIORef env
    case Map.lookup s env' of
      Just es -> lift $ putStrLn (show es)
      Nothing -> lift $ putStrLn ("'"  ++ s ++ "' not found")
    return (StrLit s)
  call "+" [IntLit i1, IntLit i2] env _ = return $ IntLit (i1+i2)
  call "+" [DoubleLit f1, DoubleLit f2] env _ = return $ DoubleLit (f1+f2)
  call "+" [StrLit i1, StrLit i2] env _ = return $ StrLit (i1++i2)
  call "+" [ListLit l1, ListLit l2] env _ = return $ ListLit (l1++l2)
  call "+" [BoolLit b1, BoolLit b2] env _ = return $ BoolLit (b1 || b2)
  call "+" [IntLit i1, DoubleLit f2] env _ = return $ DoubleLit (fromIntegral i1 + f2)
  call "+" [DoubleLit f1, IntLit i2] env _ = return $ DoubleLit (f1 + fromIntegral i2)  
  call "-" [IntLit i1, IntLit i2] env _ = return $ IntLit (i1-i2)
  call "-" [DoubleLit f1, DoubleLit f2] env _ = return $ DoubleLit (f1-f2)
  call "-" [IntLit i1, DoubleLit f2] env _ = return $ DoubleLit (fromIntegral i1 - f2)
  call "-" [DoubleLit f1, IntLit i2] env _ = return $ DoubleLit (f1 - fromIntegral i2)
  call "*" [IntLit i1, IntLit i2] env _ = return $ IntLit (i1*i2)
  call "*" [DoubleLit f1, DoubleLit f2] env _ = return $ DoubleLit (f1*f2)
  call "*" [StrLit s, IntLit i] env _ = return (StrLit $ (concatMap (\i -> s) [1..i]))
  call "*" [BoolLit b1, BoolLit b2] env _ = return $ BoolLit (b1 && b2)  
  call "*" [IntLit i1, DoubleLit f2] env _ = return $ DoubleLit (fromIntegral i1 * f2)
  call "*" [DoubleLit f1, IntLit i2] env _ = return $ DoubleLit (f1* fromIntegral i2)
  call "/" [IntLit i1, IntLit i2] env _ = return $ IntLit (i1 `div` i2)
  call "/" [DoubleLit f1, DoubleLit f2] env _ = return $ DoubleLit (f1/f2)
  call "/" [IntLit i1, DoubleLit f2] env _ = return $ DoubleLit (fromIntegral i1 / f2)
  call "/" [DoubleLit f1, IntLit i2] env _ = return $ DoubleLit (f1/ fromIntegral i2)
  call "==" [IntLit i1, IntLit i2] env _ = return $ BoolLit (i1 == i2)  
  call "==" [StrLit s1, StrLit s2] env _ = return $ BoolLit (s1 == s2)
  call "==" [BoolLit b1, BoolLit b2] env _ = return $ BoolLit (b1 == b2)  
  call "==" [ListLit l1, ListLit l2] env _ = return $ BoolLit (l1 == l2)    
  call "<" [IntLit i1, IntLit i2] env _ = return $ BoolLit (i1 < i2)  
  call "<" [DoubleLit i1, DoubleLit i2] env _ = return $ BoolLit (i1 < i2)
  call "<" [IntLit i1, DoubleLit i2] env _ = return $ BoolLit (fromIntegral i1 < i2)
  call "<" [DoubleLit i1, IntLit i2] env _ = return $ BoolLit (i1 < fromIntegral i2)
  call "<=" [IntLit i1, IntLit i2] env _ = return $ BoolLit (i1 <= i2)
  call "<=" [DoubleLit i1, DoubleLit i2] env _ = return $ BoolLit (i1 <= i2)
  call "<=" [IntLit i1, DoubleLit i2] env _ = return $ BoolLit (fromIntegral i1 <= i2)
  call "<=" [DoubleLit i1, IntLit i2] env _ = return $ BoolLit (i1 <= fromIntegral i2)
  call ">" [IntLit i1, IntLit i2] env _ = return $ BoolLit (i1 > i2)
  call ">" [DoubleLit i1, DoubleLit i2] env _ = return $ BoolLit (i1 > i2)
  call ">" [IntLit i1, DoubleLit i2] env _ = return $ BoolLit (fromIntegral i1 > i2)
  call ">" [DoubleLit i1, IntLit i2] env _ = return $ BoolLit ( i1 > fromIntegral i2)
  call ">=" [IntLit i1, IntLit i2] env _ = return $ BoolLit (i1 >= i2)
  call ">=" [DoubleLit i1, DoubleLit i2] env _ = return $ BoolLit (i1 >= i2)
  call ">=" [IntLit i1, DoubleLit i2] env _ = return $ BoolLit (fromIntegral i1 >= i2)
  call ">=" [DoubleLit i1, IntLit i2] env _ = return $ BoolLit (i1 >= fromIntegral i2)
  call "&&" [BoolLit b1, BoolLit b2] env _ = return $ BoolLit (b1 && b2)  
  call "||" [BoolLit b1, BoolLit b2] env _ = return $ BoolLit (b1 || b2)  
  call name args env c = do
    throwError ((show $ lineOfCode c) ++ ":function '" ++ name ++ " : " ++ intercalate " -> " (map dArrow (map typeOf' args)) ++ " -> ?' not found")

  makeStruct :: [Expr] -> Map Name Expr -> Env -> IO Expr
  makeStruct [] m env = return $ StructValue m
  makeStruct (StrLit name : es) m env = do
    var <- lookupVar name env
    case var of 
      Just expr -> makeStruct es (Map.insert name expr m) env
      Nothing -> error "can't find struct member"


  insertPrimitives :: Env -> IO Env
  insertPrimitives env = do
    insertFun' "debug" (Plain [Elem "String", Elem "String"]) (Call "debug") env    
    insertFun' "to_s" (Plain [Elem "Int", Elem "String"]) (Call "to_s") env
    insertFun' "to_s" (Plain [Elem "Double", Elem "String"]) (Call "to_s") env
    insertFun' "to_s" (Plain [Plain [Elem "List", Elem "a"], Elem "String"]) (Call "to_s") env
    insertFun' "to_s" (Plain [Elem "Bool", Elem "String"]) (Call "to_s") env
    insertFun' "+" (Plain [Elem "Int", Elem "Int", Elem "Int"]) (Call "+") env
    insertFun' "+" (Plain [Elem "String", Elem "String", Elem "String"]) (Call "+") env
    insertFun' "+" (Plain [Elem "Double", Elem "Double"]) (Call "+") env
    insertFun' "+" (Plain [Elem "String", Elem "String"]) (Call "+") env
    insertFun' "+" (Plain [Plain [Elem "List", Elem "a"], Plain [Elem "List", Elem "a"]]) (Call "+") env
    insertFun' "+" (Plain [Elem "Bool", Elem "Bool"]) (Call "+") env
    insertFun' "+" (Plain [Elem "Int", Elem "Double"]) (Call "+") env
    insertFun' "+" (Plain [Elem "Double", Elem "Int"]) (Call "+") env
    insertFun' "-" (Plain [Elem "Int", Elem "Int"]) (Call "-") env
    insertFun' "-" (Plain [Elem "Double", Elem "Double"]) (Call "-") env
    insertFun' "-" (Plain [Elem "Int", Elem "Double"]) (Call "-") env
    insertFun' "-" (Plain [Elem "Double", Elem "Int"]) (Call "-") env
    insertFun' "*" (Plain [Elem "Int", Elem "Int"]) (Call "*") env
    insertFun' "*" (Plain [Elem "Double", Elem "Double"]) (Call "*") env
    insertFun' "*" (Plain [Elem "String", Elem "Int"]) (Call "*") env
    insertFun' "*" (Plain [Elem "Bool", Elem "Bool"]) (Call "*") env
    insertFun' "*" (Plain [Elem "Int", Elem "Double"]) (Call "*") env
    insertFun' "*" (Plain [Elem "Double", Elem "Int"]) (Call "*") env
    insertFun' "/" (Plain [Elem "Int", Elem "Int"]) (Call "/") env
    insertFun' "/" (Plain [Elem "Double", Elem "Double"]) (Call "/") env
    insertFun' "/" (Plain [Elem "Int", Elem "Double"]) (Call "/") env
    insertFun' "/" (Plain [Elem "Double", Elem "Int"]) (Call "/") env
    insertFun' "==" (Plain [Elem "Int", Elem "Int"]) (Call "==") env
    insertFun' "==" (Plain [Elem "String", Elem "String"]) (Call "==") env
    insertFun' "==" (Plain [Elem "Bool", Elem "Bool"]) (Call "==") env
    insertFun' "==" (Plain [Plain [Elem "List", Elem "a"], Plain [Elem "List", Elem "a"]] ) (Call "==") env
    insertFun' "<" (Plain [Elem "Int", Elem "Int"]) (Call "<") env
    insertFun' "<" (Plain [Elem "Double", Elem "Double"]) (Call "<") env
    insertFun' "<" (Plain [Elem "Int", Elem "Double"]) (Call "<") env
    insertFun' "<" (Plain [Elem "Double", Elem "Int"]) (Call "<") env
    insertFun' "<=" (Plain [Elem "Int", Elem "Int"]) (Call "<=") env
    insertFun' "<=" (Plain [Elem "Double", Elem "Double"]) (Call "<=") env
    insertFun' "<=" (Plain [Elem "Int", Elem "Double"]) (Call "<=") env
    insertFun' "<=" (Plain [Elem "Double", Elem "Int"]) (Call "<=") env
    insertFun' ">" (Plain [Elem "Int", Elem "Int"]) (Call ">") env
    insertFun' ">" (Plain [Elem "Double", Elem "Double"]) (Call ">") env
    insertFun' ">" (Plain [Elem "Int", Elem "Double"]) (Call ">") env
    insertFun' ">" (Plain [Elem "Double", Elem "Int"]) (Call ">") env
    insertFun' ">=" (Plain [Elem "Int", Elem "Int"]) (Call ">=") env
    insertFun' ">=" (Plain [Elem "Double", Elem "Double"]) (Call ">=") env
    insertFun' ">=" (Plain [Elem "Int", Elem "Double"]) (Call ">=") env
    insertFun' ">=" (Plain [Elem "Double", Elem "Int"]) (Call ">=") env
    insertFun' "&&" (Plain [Elem "Bool", Elem "Bool"]) (Call "&&") env
    insertFun' "||" (Plain [Elem "Bool", Elem "Bool"]) (Call "||") env