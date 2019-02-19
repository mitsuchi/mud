module Primitive where

  import Control.Monad.Except
  import Data.List
  import Data.Map as Map hiding (map)
  import Data.IORef

  import RecList
  import Env
  import Expr
  import TypeUtil
  
  call :: Name -> [Expr] -> Env -> Code -> ExceptT String IO Expr
  call "head" [ListLit (e:es) _] env _ = return e
  call "tail" [ListLit (e:es) c] env _ = return $ ListLit es c
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
  call "to_s" [ListLit l _] env _ = return $ StrLit (show l)
  call "to_s" [BoolLit b] env _ = return $ StrLit (show b)
  call "debug" [StrLit "env", StrLit s] env _ = do
    env' <- liftIO $ readIORef env
    case Map.lookup s env' of
      Just es -> lift $ putStrLn (show es)
      Nothing -> lift $ putStrLn ("'"  ++ s ++ "' not found")
    return (StrLit s)
  call "+" [IntLit i1, IntLit i2] env _ = return $ IntLit (i1+i2)
  call "+" [DoubleLit f1, DoubleLit f2] env _ = return $ DoubleLit (f1+f2)
  call "+" [StrLit i1, StrLit i2] env _ = return $ StrLit (i1++i2)
  call "+" [ListLit l1 c1, ListLit l2 c2] env _ = return $ ListLit (l1++l2) c1
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
  call "==" [ListLit l1 _, ListLit l2 _] env _ = return $ BoolLit (l1 == l2)    
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
    insertCall "puts" (Elems [Elem "a", Elem "String"]) env    
    insertCall "head" (Elems [Elems [Elem "List", Elem "a"], Elem "a"]) env    
    insertCall "tail" (Elems [Elems [Elem "List", Elem "a"], Elems [Elem "List", Elem "a"]]) env    
    insertCall "debug" (Elems [Elem "String", Elem "String", Elem "String"]) env    
    insertCall "to_s" (Elems [Elem "Int", Elem "String"]) env
    insertCall "to_s" (Elems [Elem "Double", Elem "String"]) env
    insertCall "to_s" (Elems [Elems [Elem "List", Elem "a"], Elem "String"]) env
    insertCall "to_s" (Elems [Elem "Bool", Elem "String"]) env
    insertCall "+" (Elems [Elem "Int", Elem "Int", Elem "Int"]) env
    insertCall "+" (Elems [Elem "String", Elem "String", Elem "String"]) env
    insertCall "+" (Elems [Elem "Double", Elem "Double", Elem "Double"]) env
    insertCall "+" (Elems [Elem "String", Elem "String", Elem "String"]) env
    insertCall "+" (Elems [Elem "Bool", Elem "Bool", Elem "Bool"]) env
    insertCall "+" (Elems [Elem "Int", Elem "Double", Elem "Double"]) env
    insertCall "+" (Elems [Elem "Double", Elem "Int", Elem "Double"]) env
    insertCall "+" (Elems [Elem "a", Elem "a", Elem "a"]) env            
    insertCall "-" (Elems [Elem "Int", Elem "Int", Elem "Int"]) env
    insertCall "-" (Elems [Elem "Double", Elem "Double", Elem "Double"]) env
    insertCall "-" (Elems [Elem "Int", Elem "Double", Elem "Double"]) env
    insertCall "-" (Elems [Elem "Double", Elem "Int", Elem "Double"]) env
    insertCall "-" (Elems [Elem "a", Elem "a", Elem "a"]) env        
    insertCall "*" (Elems [Elem "Int", Elem "Int", Elem "Int"]) env
    insertCall "*" (Elems [Elem "Double", Elem "Double", Elem "Double"]) env
    insertCall "*" (Elems [Elem "String", Elem "Int", Elem "String"]) env
    insertCall "*" (Elems [Elem "Bool", Elem "Bool", Elem "Bool"]) env
    insertCall "*" (Elems [Elem "Int", Elem "Double", Elem "Double"]) env
    insertCall "*" (Elems [Elem "Double", Elem "Int", Elem "Double"]) env
    insertCall "/" (Elems [Elem "Int", Elem "Int", Elem "Int"]) env
    insertCall "/" (Elems [Elem "Double", Elem "Double", Elem "Double"]) env
    insertCall "/" (Elems [Elem "Int", Elem "Double", Elem "Double"]) env
    insertCall "/" (Elems [Elem "Double", Elem "Int", Elem "Double"]) env
    insertCall "==" (Elems [Elem "Int", Elem "Int", Elem "Bool"]) env
    insertCall "==" (Elems [Elem "Double", Elem "Double", Elem "Bool"]) env
    insertCall "==" (Elems [Elem "String", Elem "String", Elem "Bool"]) env
    insertCall "==" (Elems [Elem "Bool", Elem "Bool", Elem "Bool"]) env
    insertCall "==" (Elems [Elems [Elem "List", Elem "a"], Elems [Elem "List", Elem "a"], Elem "Bool"] ) env
    insertCall "==" (Elems [Elem "a", Elem "a", Elem "Bool"]) env
    insertCall "<" (Elems [Elem "Int", Elem "Int", Elem "Bool"]) env
    insertCall "<" (Elems [Elem "Double", Elem "Double", Elem "Bool"]) env
    insertCall "<" (Elems [Elem "Int", Elem "Double", Elem "Bool"]) env
    insertCall "<" (Elems [Elem "Double", Elem "Int", Elem "Bool"]) env
    insertCall "<" (Elems [Elem "a", Elem "a", Elem "Bool"]) env
    insertCall "<=" (Elems [Elem "Int", Elem "Int", Elem "Bool"]) env
    insertCall "<=" (Elems [Elem "Double", Elem "Double", Elem "Bool"]) env
    insertCall "<=" (Elems [Elem "Int", Elem "Double", Elem "Bool"]) env
    insertCall "<=" (Elems [Elem "Double", Elem "Int", Elem "Bool"]) env
    insertCall "<=" (Elems [Elem "a", Elem "a", Elem "Bool"]) env    
    insertCall ">" (Elems [Elem "Int", Elem "Int", Elem "Bool"]) env
    insertCall ">" (Elems [Elem "Double", Elem "Double", Elem "Bool"]) env
    insertCall ">" (Elems [Elem "Int", Elem "Double", Elem "Bool"]) env
    insertCall ">" (Elems [Elem "Double", Elem "Int", Elem "Bool"]) env
    insertCall ">" (Elems [Elem "a", Elem "a", Elem "Bool"]) env    
    insertCall ">=" (Elems [Elem "Int", Elem "Int", Elem "Bool"]) env
    insertCall ">=" (Elems [Elem "Double", Elem "Double", Elem "Bool"]) env
    insertCall ">=" (Elems [Elem "Int", Elem "Double", Elem "Bool"]) env
    insertCall ">=" (Elems [Elem "Double", Elem "Int", Elem "Bool"]) env
    insertCall ">=" (Elems [Elem "a", Elem "a", Elem "Bool"]) env    
    insertCall "&&" (Elems [Elem "Bool", Elem "Bool", Elem "Bool"]) env
    insertCall "||" (Elems [Elem "Bool", Elem "Bool", Elem "Bool"]) env

  insertCall :: String -> RecList Type -> Env -> IO Env
  insertCall name types env = insertFun' name types (Call name (generalizeTypeSig types)) env