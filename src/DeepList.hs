module DeepList where
  import Data.List

  data DeepList a = Elem a | Plain [DeepList a] deriving Show

  dInit :: DeepList a -> DeepList a
  dInit (Elem x) = Elem x
  dInit (Plain xs) = Plain (init xs)

  dLast :: DeepList a -> DeepList a
  dLast (Elem x) = Elem x
  dLast (Plain xs) = last xs

  dShow :: (Show a) => DeepList a -> String
  dShow (Elem x) = show x
  dShow (Plain xs) = "[" ++ intercalate "," (map dShow xs) ++ "]"

  -- dArrow :: (Show a) => DeepList a -> String
  -- dArrow (Elem x) = show x
  -- dArrow (Plain xs) = intercalate "->" (map dArrow xs)

  -- listify :: (Show a) => DeepList a -> [String]
  -- listify (Elem x) = [show x]
  -- listify (Plain xs) = map dArrow xs

  dArrow :: DeepList String -> String
  dArrow (Elem x) = x
  dArrow (Plain xs) = "(" ++ intercalate "->" (map dArrow xs) ++ ")"

  listify :: DeepList String -> [String]
  listify (Elem x) = [x]
  listify (Plain xs) = map dArrow xs

  dAppend :: DeepList a -> DeepList a -> DeepList a
  dAppend (Elem e) (Plain []) = Plain [Elem e]
  dAppend (Elem e) (Plain es) = Plain ((Elem e):es)
  dAppend (Plain es1) (Plain es2) = Plain (es1++es2)

  dFlatten :: DeepList a -> [a]
  dFlatten (Elem e) = [e]
  dFlatten (Plain []) = []
  dFlatten (Plain (e:es)) = (dFlatten e) ++ (dFlatten (Plain es))