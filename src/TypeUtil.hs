module TypeUtil where

  import Data.Char
  import qualified Data.Map as M
  import DeepList

  findTypeEnv :: DeepList String -> DeepList String -> M.Map String (DeepList String) -> Bool -> Maybe (M.Map String (DeepList String))
  --findTypeEnv (Elem a) (Elem b) env strict | (isUpper (a!!0) && isUpper (b!!0)) = Just env
  findTypeEnv (Elem a) (Elem b) env strict | (not strict && isUpper (a!!0) && isUpper (b!!0) ) || strict =
    if a == b then (Just env) else Nothing
  findTypeEnv (Elem a) (Elem b) env strict | (not strict && isLower (b!!0) ) || strict =
    Just env
  findTypeEnv (Elem a) (Plain bs) env strict | (not strict && isUpper (a!!0) ) || strict = Nothing
  findTypeEnv (Elem "_") (Plain bs) env strict = Nothing
  findTypeEnv (Elem a) b env strict | (not strict && isLower (a!!0) ) || strict = 
    let mapped = M.lookup a env in
    if mapped == Nothing then Just (M.insert a b env)
    else if mapped == Just b then Just env 
    else Nothing
  findTypeEnv (Plain as) (Elem b) env strict = Nothing
  findTypeEnv (Plain []) (Plain bs) env strict = Just env
  findTypeEnv (Plain as) (Plain []) env strict = Just env
  --findTypeEnv (Plain (a:[])) (Plain (b:[])) env strict = Just env
  findTypeEnv (Plain (a:as)) (Plain (b:bs)) env strict =
    case findTypeEnv a b env strict of
      Nothing -> Nothing
      Just env' -> findTypeEnv (Plain as) (Plain bs) env' strict
  findTypeEnv t1 t2 env strict = error ("findTypeEnv fail, t1 = " ++ (show t1) ++ ", t2 = " ++ (show t2))  

  isConcrete :: DeepList String -> Bool
  isConcrete (Elem a) = (a == "List") || isUpper (a!!0)
  isConcrete (Plain xs) = and (map isConcrete xs)

  isVariable :: DeepList String -> Bool
  isVariable (Elem a) = (a == "List") || isLower (a!!0)
  isVariable (Plain xs) = and (map isVariable xs)

  -- 型にひとつでも型変数を含むか？
  hasVariable :: DeepList String -> Bool
  hasVariable (Elem a) = isLower (a!!0)
  hasVariable (Plain xs) = or (map isVariable xs)  

  generalizeTypeSig :: DeepList String -> DeepList String
  generalizeTypeSig list = gnrlize' list (makeMap (dFlatten list))

  gnrlize' :: DeepList String -> M.Map String Int -> DeepList String
  gnrlize' (Elem e) table = case M.lookup e table of
    Nothing -> Elem e
    Just i -> Elem ("t" ++ show i)
  gnrlize' (Plain []) table = Plain []
  gnrlize' (Plain (e:es)) table = 
    let (Plain rest') = (gnrlize' (Plain es) table)
    in Plain ((gnrlize' e table) : rest')

  makeMap :: [String] -> M.Map String Int
  makeMap list = makeMap' list 0 M.empty

  makeMap' :: [String] -> Int -> M.Map String Int -> M.Map String Int
  makeMap' [] num table = table
  makeMap' (e:es) num table = if isUpper(e !! 0) then (makeMap' es num table) else 
    case M.lookup e table of
      Nothing -> makeMap' es (num+1) (M.insert e num table)
      Just i  -> makeMap' es num table


  -- [("r",Plain [Elem "Int"]),("i",Plain [Elem "Int"])]
  -- を以下に変換する
  -- Plain [Elem "Int", Elem "Int"]
  typeDefToTypes :: [(String, DeepList String)] -> DeepList String
  typeDefToTypes es = Plain (foldMap ((++) . (\(Plain x) -> x) . snd) es [])

  generalizeTypesWith :: String -> DeepList String -> DeepList String
  generalizeTypesWith str list = gnrlizeWith str list (makeMap (dFlatten list))

  gnrlizeWith :: String -> DeepList String -> M.Map String Int -> DeepList String
  gnrlizeWith str (Elem e) table = case M.lookup e table of
    Nothing -> Elem e
    Just i -> Elem (str ++ show i)
  gnrlizeWith str (Plain []) table = Plain []
  gnrlizeWith str (Plain (e:es)) table = 
    let (Plain rest') = (gnrlizeWith str (Plain es) table)
    in Plain ((gnrlizeWith str e table) : rest')


  -- 関数の型と実際の引数の型との対応をもとに、型環境を作る
  -- 引数
  -- 1. 関数の型のうち、返り値をのぞいた型のリスト。例：(a->b)->a->b なら [Plain [Elem "a", Elem "b"], Elem "a"]
  -- 2. 引数の型のリスト。例：[Elem "String", Elem "Int"]
  -- あれそれって、findTypeEnv だな。でした。
  -- でも別につくってみる。対応が矛盾する場合には Nothing を返す。あれば Just 型環境を返す（空かもしれない）
  unify :: DeepList String -> DeepList String -> M.Map String (DeepList String) -> Maybe (M.Map String (DeepList String))
  unify a b env | isConcrete a && isConcrete b = 
    -- 両方とも具体型の場合は、一致するかどうかを見る
    if a == b then Just env else Nothing
  unify (Elem a) b env | isLower(a!!0) =
    -- 関数が型変数の場合は、環境に a:b を追加する。衝突する場合は衝突処理を行う。
    -- ただし a = b の自明な場合は追加しない
    if (Elem a) == b then Just env else push a b env
  unify a (Elem b) env | isLower(b!!0) =
    -- 引数が型変数の場合は、環境に b:a を追加する。衝突する場合は衝突処理を行う。
    push b a env
  unify (Plain (a:as)) (Plain (b:bs)) env =
    -- リストどうしの場合は、最初の要素を unify して、成功するなら残りを unify する
    case unify a b env of
      Nothing -> Nothing
      Just env' -> unify (Plain as) (Plain bs) env'
  unify _ _ _ = Nothing -- 上記以外の場合は失敗

  push :: String -> DeepList String -> M.Map String (DeepList String) -> Maybe (M.Map String (DeepList String))
  push t x env = 
    -- 環境から ft をキーとする型を引いてくる
    case M.lookup t env of
      -- なにもなければ、単に環境に追加する
      Nothing -> Just (M.insert t x env)
      -- 既存に登録(x0)がある場合は、既存と新規の型の種類に応じて場合分けする
      Just x0 -> case (x0, x) of
        (x0, x)
          -- x0が具体型、xが具体型の場合。
          -- x0 と x が一致するなら問題なし。Eをそのまま返す。
          -- 一致しないなら問題あり。Nothingを返す。
          | isConcrete(x0) && isConcrete(x) -> if x0 == x then Just env else Nothing 
          -- x0が具体型、xが型変数の場合
          -- E(t:x0) を E(t:x0) + x:x0 とする。この処理で x が衝突する場合は再び衝突処理を行う。
          | isConcrete(x0) && isVariable(x) -> unify x x0 env
          -- x0が型変数、xが具体型の場合
          -- E(t:x0) を E(t:x) + x0:x とする。必要なら衝突処理を行う。
          | isVariable(x0) && isConcrete(x) -> unify x0 x (M.insert t x env)
          -- x0が型変数、xが型変数の場合
          -- E(t:x0) を E(t:x0) + x0:x とする。必要なら衝突処理を行う。
          | isVariable(x0) && isVariable(x) -> unify x0 x env