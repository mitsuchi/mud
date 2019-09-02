-- 任意の深さになりうる、再帰的なリスト
-- 型の表現に使う: Int -> (Int->Int) など
module RecList where
import           Data.List

data RecList a = Elem a | Elems [RecList a] deriving Show

-- 再帰的なリストの末尾をのぞいたものをリストとして返す
rArgs :: RecList a -> [RecList a]
rArgs (Elem x)   = [Elem x]
rArgs (Elems xs) = init xs

-- 末尾をのぞいたものを再帰的なリストとして返す
rInit :: RecList a -> RecList a
rInit (Elem x)   = Elem x
rInit (Elems xs) = Elems (init xs)

-- 末尾
rLast :: RecList a -> RecList a
rLast (Elem x)   = Elem x
rLast (Elems xs) = last xs

-- リスト文字列の形で表示
rShow :: (Show a) => RecList a -> String
rShow (Elem x)   = show x
rShow (Elems xs) = "[" ++ intercalate "," (map rShow xs) ++ "]"

-- 型の表記として表示
-- 例：[Int,[Int, String]] -> Int -> (Int->String)
argSig :: RecList String -> String
argSig (Elem x)   = x
argSig (Elems xs) = intercalate " -> " (map rArrow (init xs)) ++ " -> ?"

-- 型の表記として表示
-- 例：[Int,[Int, String]] -> (Int -> (Int->String))
rArrow :: RecList String -> String
rArrow (Elem x) = x
rArrow (Elems [Elem "List", Elem e]) = "[" ++ e ++ "]"
rArrow (Elems xs) = "(" ++ intercalate " -> " (map rArrow xs) ++ ")"

-- 各要素を型表記したようなリストに変換する
-- 例：[Int,[Int, String]] -> [Int, Int->String]
listify :: RecList String -> [String]
listify (Elem x)   = [x]
listify (Elems xs) = map rArrow xs

-- 再帰的なリスト同士をつなげる
-- 例：[a,[b,c]] `rAppend` [d] -> [a,[b,c],d]
rAppend :: RecList a -> RecList a -> RecList a
rAppend (Elem e) (Elems [])     = Elems [Elem e]
rAppend (Elem e) (Elems es)     = Elems (Elem e:es)
rAppend (Elems es1) (Elems es2) = Elems (es1++es2)

-- フラットなリストに戻す
-- 例：[a,[b,c]] -> [a,b,c]
rFlatten :: RecList a -> [a]
rFlatten (Elem e)       = [e]
rFlatten (Elems [])     = []
rFlatten (Elems (e:es)) = rFlatten e ++ rFlatten (Elems es)

-- 対応するそれぞれの要素がすべて等しいなら、再帰的なリスト全体が等しい
instance (Eq a) => Eq (RecList a) where
  (Elem x) == (Elem y) = x == y
  (Elem x) == (Elems y) = False
  (Elems x) == (Elem y) = False
  (Elems x) == (Elems y) = x == y
