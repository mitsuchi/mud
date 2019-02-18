module RecList where
  import Data.List

  data RecList a = Elem a | Elems [RecList a] deriving Show

  dArgs :: RecList a -> [RecList a]
  dArgs (Elem x) = [Elem x]
  dArgs (Elems xs) = init xs

  dInit :: RecList a -> RecList a
  dInit (Elem x) = Elem x
  dInit (Elems xs) = Elems (init xs)

  dLast :: RecList a -> RecList a
  dLast (Elem x) = Elem x
  dLast (Elems xs) = last xs

  dShow :: (Show a) => RecList a -> String
  dShow (Elem x) = show x
  dShow (Elems xs) = "[" ++ intercalate "," (map dShow xs) ++ "]"

  argSig :: RecList String -> String
  argSig (Elem x) = x
  argSig (Elems xs) = (intercalate " -> " (map dArrow (init xs))) ++ " -> ?"

  dArrow :: RecList String -> String
  dArrow (Elem x) = x
  dArrow (Elems [Elem "List", Elem e]) = "[" ++ e ++ "]"
  dArrow (Elems xs) = "(" ++ intercalate " -> " (map dArrow xs) ++ ")"

  listify :: RecList String -> [String]
  listify (Elem x) = [x]
  listify (Elems xs) = map dArrow xs

  dAppend :: RecList a -> RecList a -> RecList a
  dAppend (Elem e) (Elems []) = Elems [Elem e]
  dAppend (Elem e) (Elems es) = Elems ((Elem e):es)
  dAppend (Elems es1) (Elems es2) = Elems (es1++es2)

  dFlatten :: RecList a -> [a]
  dFlatten (Elem e) = [e]
  dFlatten (Elems []) = []
  dFlatten (Elems (e:es)) = (dFlatten e) ++ (dFlatten (Elems es))

  instance (Eq a) => Eq (RecList a) where
    (Elem x) == (Elem y) = (x == y)
    (Elem x) == (Elems y) = False
    (Elems x) == (Elem y) = False
    (Elems x) == (Elems y) = (x == y)
