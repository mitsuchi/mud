fun insert : a -> [a] -> [a] = {
  x     [] -> [x]
  x [e;es] -> if x < e
    then [x] + [e] + es
    else [e] + (insert x es)
}

fun foldr : (a->b->b) -> b -> [a] -> b = {
  f a []      -> a
  f a [x;xs]  -> f x (foldr f a xs)
}

foldr insert [] [1,2,3]

fun isort : [a] -> [a] = {
  es -> foldr insert [] es
}
