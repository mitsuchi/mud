module TypeUtil where

  import Control.Monad.Except
  import Data.Char
  import Data.Map as Map
  import RecList

  type IOThrowsError = ExceptT String IO

  isConcrete :: RecList String -> Bool
  isConcrete (Elem a) = (a == "List") || isUpper (a!!0)
  isConcrete (Elems xs) = and (Prelude.map isConcrete xs)

  isVariable :: RecList String -> Bool
  isVariable (Elem a) = (a == "List") || isLower (a!!0)
  isVariable (Elems xs) = and (Prelude.map isVariable xs)

  -- 型にひとつでも型変数を含むか？
  hasVariable :: RecList String -> Bool
  hasVariable (Elem a) = isLower (a!!0)
  hasVariable (Elems xs) = or (Prelude.map isVariable xs)  

  generalizeTypes :: RecList String -> RecList String
  generalizeTypes list = gnrlize' list (makeMap (rFlatten list))

  gnrlize' :: RecList String -> Map String Int -> RecList String
  gnrlize' (Elem e) table = case Map.lookup e table of
    Nothing -> Elem e
    Just i -> Elem ("t" ++ show i)
  gnrlize' (Elems []) table = Elems []
  gnrlize' (Elems (e:es)) table = 
    let (Elems rest') = (gnrlize' (Elems es) table)
    in Elems ((gnrlize' e table) : rest')

  makeMap :: [String] -> Map String Int
  makeMap list = makeMap' list 0 Map.empty

  makeMap' :: [String] -> Int -> Map String Int -> Map String Int
  makeMap' [] num table = table
  makeMap' (e:es) num table = if isUpper(e !! 0) then (makeMap' es num table) else 
    case Map.lookup e table of
      Nothing -> makeMap' es (num+1) (Map.insert e num table)
      Just i  -> makeMap' es num table


  -- [("r",Elems [Elem "Int"]),("i",Elems [Elem "Int"])] "Complex"
  -- を以下に変換する
  -- Elems [Elem "Int", Elem "Int", Elem "Complex"]
  typeDefToTypes :: [(String, RecList String)] -> String -> RecList String
  typeDefToTypes es name = Elems ((foldMap ((++) . (\(Elems x) -> x) . snd) es []) ++ [Elem name])

  generalizeTypesWith :: String -> RecList String -> RecList String
  generalizeTypesWith str list = gnrlizeWith str list (makeMap (rFlatten list))

  gnrlizeWith :: String -> RecList String -> Map String Int -> RecList String
  gnrlizeWith str (Elem e) table = case Map.lookup e table of
    Nothing -> Elem e
    Just i -> Elem (str ++ show i)
  gnrlizeWith str (Elems []) table = Elems []
  gnrlizeWith str (Elems (e:es)) table = 
    let (Elems rest') = (gnrlizeWith str (Elems es) table)
    in Elems ((gnrlizeWith str e table) : rest')


  -- 関数の型と実際の引数の型との対応をもとに、型環境を作る
  -- 引数
  -- 1. 関数の型のうち、返り値をのぞいた型のリスト。例：(a->b)->a->b なら [Elems [Elem "a", Elem "b"], Elem "a"]
  -- 2. 引数の型のリスト。例：[Elem "String", Elem "Int"]
  -- 対応が矛盾する場合には Nothing を返す。あれば Just 型環境を返す（空かもしれない）
  unify :: RecList String -> RecList String -> Map String (RecList String) -> Maybe (Map String (RecList String))
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
  unify (Elems (a:as)) (Elems (b:bs)) env =
    -- リストどうしの場合は、最初の要素を unify して、成功するなら残りを unify する
    case unify a b env of
      Nothing -> Nothing
      Just env' -> unify (Elems as) (Elems bs) env'
  unify (Elem "_") b env = Just Map.empty
  unify _ _ _ = Nothing -- 上記以外の場合は失敗

  push :: String -> RecList String -> Map String (RecList String) -> Maybe (Map String (RecList String))
  push t x env = 
    -- 環境から ft をキーとする型を引いてくる
    case Map.lookup t env of
      -- なにもなければ、単に環境に追加する
      Nothing -> Just (Map.insert t x env)
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
          | isVariable(x0) && isConcrete(x) -> unify x0 x (Map.insert t x env)
          -- x0が型変数、xが型変数の場合
          -- E(t:x0) を E(t:x0) + x0:x とする。必要なら衝突処理を行う。
          | isVariable(x0) && isVariable(x) -> unify x0 x env