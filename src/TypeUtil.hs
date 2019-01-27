module TypeUtil where

  import Data.Char
  import Data.Map as Map hiding (map)
  import DeepList

  findTypeEnv :: DeepList String -> DeepList String -> Map String (DeepList String) -> Maybe (Map String (DeepList String))
  findTypeEnv (Elem a) (Elem b) env | isUpper (a!!0) && isUpper (b!!0) =
    if a == b then (Just env) else Nothing
  findTypeEnv (Elem a) (Elem b) env | isLower (b!!0) =
    Just env
  findTypeEnv (Elem a) (Plain bs) env | isUpper (a!!0) = Nothing
  findTypeEnv (Elem a) b env | isLower (a!!0) = 
    let mapped = Map.lookup a env in
    if mapped == Nothing then Just (Map.insert a b env)
    else if mapped == Just b then Just env 
    else Nothing
  findTypeEnv (Plain as) (Elem b) env = Nothing
  findTypeEnv (Plain []) (Plain bs) env = Just env
  findTypeEnv (Plain as) (Plain []) env = Just env
  findTypeEnv (Plain (a:as)) (Plain (b:bs)) env =
    case findTypeEnv a b env of
      Nothing -> Nothing
      Just env' -> findTypeEnv (Plain as) (Plain bs) env'
  findTypeEnv t1 t2 env = error ("findTypeEnv fail, t1 = " ++ (show t1) ++ ", t2 = " ++ (show t2))  
  

  isConcrete :: DeepList String -> Bool
  isConcrete (Elem a) = isUpper (a!!0)
  isConcrete (Plain xs) = and (map isConcrete xs)
