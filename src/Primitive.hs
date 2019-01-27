module Primitive where

  import Data.Map as Map
  import Data.IORef

  import Env
  import Expr
  
  call :: Name -> [Expr] -> Env -> IO Expr
  call "head" [ListLit (e:es)] env = return e
  call "tail" [ListLit (e:es)] env = return $ ListLit es
  call "puts" [e] env = do
    print e
    return e
  call "makeStruct" (name:args) env = do
    makeStruct args (Map.fromList [("type", name)]) env
  call "lookupStruct" [StructValue structValue, StrLit member] env = do
    case Map.lookup member structValue of
      Just expr -> return expr
      Nothing -> error ("can't find struct member '" ++ member ++ "'")
  call "+" [IntLit i1, IntLit i2] env = return $ IntLit (i1+i2)
  call "+" [DoubleLit f1, DoubleLit f2] env = return $ DoubleLit (f1+f2)
  call "+" [StrLit i1, StrLit i2] env = return $ StrLit (i1++i2)
  call "+" [ListLit l1, ListLit l2] env = return $ ListLit (l1++l2)
  call "+" [BoolLit b1, BoolLit b2] env = return $ BoolLit (b1 || b2)
  call "+" [IntLit i1, DoubleLit f2] env = return $ DoubleLit (fromIntegral i1 + f2)
  call "+" [DoubleLit f1, IntLit i2] env = return $ DoubleLit (f1 + fromIntegral i2)  
  call "-" [IntLit i1, IntLit i2] env = return $ IntLit (i1-i2)
  call "-" [DoubleLit f1, DoubleLit f2] env = return $ DoubleLit (f1-f2)
  call "-" [IntLit i1, DoubleLit f2] env = return $ DoubleLit (fromIntegral i1 - f2)
  call "-" [DoubleLit f1, IntLit i2] env = return $ DoubleLit (f1 - fromIntegral i2)
  call "*" [IntLit i1, IntLit i2] env = return $ IntLit (i1*i2)
  call "*" [DoubleLit f1, DoubleLit f2] env = return $ DoubleLit (f1*f2)
  call "*" [StrLit s, IntLit i] env = return (StrLit $ (concatMap (\i -> s) [1..i]))
  call "*" [BoolLit b1, BoolLit b2] env = return $ BoolLit (b1 && b2)  
  call "*" [IntLit i1, DoubleLit f2] env = return $ DoubleLit (fromIntegral i1 * f2)
  call "*" [DoubleLit f1, IntLit i2] env = return $ DoubleLit (f1* fromIntegral i2)
  call "/" [IntLit i1, IntLit i2] env = return $ IntLit (i1 `div` i2)
  call "/" [DoubleLit f1, DoubleLit f2] env = return $ DoubleLit (f1/f2)
  call "/" [IntLit i1, DoubleLit f2] env = return $ DoubleLit (fromIntegral i1 / f2)
  call "/" [DoubleLit f1, IntLit i2] env = return $ DoubleLit (f1/ fromIntegral i2)
  call "==" [IntLit i1, IntLit i2] env = return $ BoolLit (i1 == i2)  
  call "==" [StrLit s1, StrLit s2] env = return $ BoolLit (s1 == s2)
  call "==" [BoolLit b1, BoolLit b2] env = return $ BoolLit (b1 == b2)  
  call "==" [ListLit l1, ListLit l2] env = return $ BoolLit (l1 == l2)    
  call "<" [IntLit i1, IntLit i2] env = return $ BoolLit (i1 < i2)  
  call "<" [DoubleLit i1, DoubleLit i2] env = return $ BoolLit (i1 < i2)
  call "<" [IntLit i1, DoubleLit i2] env = return $ BoolLit (fromIntegral i1 < i2)
  call "<" [DoubleLit i1, IntLit i2] env = return $ BoolLit (i1 < fromIntegral i2)
  call "<=" [IntLit i1, IntLit i2] env = return $ BoolLit (i1 <= i2)
  call "<=" [DoubleLit i1, DoubleLit i2] env = return $ BoolLit (i1 <= i2)
  call "<=" [IntLit i1, DoubleLit i2] env = return $ BoolLit (fromIntegral i1 <= i2)
  call "<=" [DoubleLit i1, IntLit i2] env = return $ BoolLit (i1 <= fromIntegral i2)
  call ">" [IntLit i1, IntLit i2] env = return $ BoolLit (i1 > i2)
  call ">" [DoubleLit i1, DoubleLit i2] env = return $ BoolLit (i1 > i2)
  call ">" [IntLit i1, DoubleLit i2] env = return $ BoolLit (fromIntegral i1 > i2)
  call ">" [DoubleLit i1, IntLit i2] env = return $ BoolLit ( i1 > fromIntegral i2)
  call ">=" [IntLit i1, IntLit i2] env = return $ BoolLit (i1 >= i2)
  call ">=" [DoubleLit i1, DoubleLit i2] env = return $ BoolLit (i1 >= i2)
  call ">=" [IntLit i1, DoubleLit i2] env = return $ BoolLit (fromIntegral i1 >= i2)
  call ">=" [DoubleLit i1, IntLit i2] env = return $ BoolLit (i1 >= fromIntegral i2)
  call "&&" [BoolLit b1, BoolLit b2] env = return $ BoolLit (b1 && b2)  
  call "||" [BoolLit b1, BoolLit b2] env = return $ BoolLit (b1 || b2)  
  call name args env = do
    env' <- readIORef env
    error (name ++ " not found, args = " ++ (show args) ++ ", env = " ++ (show env'))

  makeStruct :: [Expr] -> Map Name Expr -> Env -> IO Expr
  makeStruct [] m env = return $ StructValue m
  makeStruct (StrLit name : es) m env = do
    var <- lookupVar name env
    case var of 
      Just expr -> makeStruct es (Map.insert name expr m) env
      Nothing -> error "can't find struct member"