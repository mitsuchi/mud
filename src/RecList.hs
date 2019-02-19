module RecList where
  import Data.List

  data RecList a = Elem a | Elems [RecList a] deriving Show

  rArgs :: RecList a -> [RecList a]
  rArgs (Elem x) = [Elem x]
  rArgs (Elems xs) = init xs

  rInit :: RecList a -> RecList a
  rInit (Elem x) = Elem x
  rInit (Elems xs) = Elems (init xs)

  rLast :: RecList a -> RecList a
  rLast (Elem x) = Elem x
  rLast (Elems xs) = last xs

  rShow :: (Show a) => RecList a -> String
  rShow (Elem x) = show x
  rShow (Elems xs) = "[" ++ intercalate "," (map rShow xs) ++ "]"

  argSig :: RecList String -> String
  argSig (Elem x) = x
  argSig (Elems xs) = (intercalate " -> " (map rArrow (init xs))) ++ " -> ?"

  rArrow :: RecList String -> String
  rArrow (Elem x) = x
  rArrow (Elems [Elem "List", Elem e]) = "[" ++ e ++ "]"
  rArrow (Elems xs) = "(" ++ intercalate " -> " (map rArrow xs) ++ ")"

  listify :: RecList String -> [String]
  listify (Elem x) = [x]
  listify (Elems xs) = map rArrow xs

  rAppend :: RecList a -> RecList a -> RecList a
  rAppend (Elem e) (Elems []) = Elems [Elem e]
  rAppend (Elem e) (Elems es) = Elems ((Elem e):es)
  rAppend (Elems es1) (Elems es2) = Elems (es1++es2)

  rFlatten :: RecList a -> [a]
  rFlatten (Elem e) = [e]
  rFlatten (Elems []) = []
  rFlatten (Elems (e:es)) = (rFlatten e) ++ (rFlatten (Elems es))

  instance (Eq a) => Eq (RecList a) where
    (Elem x) == (Elem y) = (x == y)
    (Elem x) == (Elems y) = False
    (Elems x) == (Elem y) = False
    (Elems x) == (Elems y) = (x == y)
