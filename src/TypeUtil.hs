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
  findTypeEnv (Elem "_") (Plain bs) env = Nothing
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

  generalizeTypeSig :: DeepList String -> DeepList String
  generalizeTypeSig list = gnrlize' list (makeMap (dFlatten list))

  gnrlize' :: DeepList String -> Map String Int -> DeepList String
  gnrlize' (Elem e) table = case Map.lookup e table of
    Nothing -> Elem e
    Just i -> Elem ("t" ++ show i)
  gnrlize' (Plain []) table = Plain []
  gnrlize' (Plain (e:es)) table = 
    let (Plain rest') = (gnrlize' (Plain es) table)
    in Plain ((gnrlize' e table) : rest')

  makeMap :: [String] -> Map String Int
  makeMap list = makeMap' list 0 Map.empty

  makeMap' :: [String] -> Int -> Map String Int -> Map String Int
  makeMap' [] num table = table
  makeMap' (e:es) num table = if isUpper(e !! 0) then (makeMap' es num table) else 
    case Map.lookup e table of
      Nothing -> makeMap' es (num+1) (Map.insert e num table)
      Just i  -> makeMap' es num table


  -- [("r",Plain [Elem "Int"]),("i",Plain [Elem "Int"])]
  -- を以下に変換する
  -- Plain [Elem "Int", Elem "Int"]
  typeDefToTypes :: [(String, DeepList String)] -> DeepList String
  typeDefToTypes es = Plain (foldMap ((++) . (\(Plain x) -> x) . snd) es [])
