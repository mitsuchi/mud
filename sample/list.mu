fun length : [a] -> Int = {
  []    -> 0
  [h,t] -> 1 + length t
}

fun map : [a] -> (a->b) -> [b] = {
  []    f -> []
  [h,t] f -> [f h] + map t f
}

fun reverse : [a] -> [a] = {
  []    -> []
  [h,t] -> reverse t + [h]
}

fun uniq : [a] -> [a] = {
  []     -> []
  [e,es] -> if es == []
    then [e]
    else if (e == es.head)
      then (uniq es)
      else [e] + (uniq es)
}

fun select : [a] -> (a -> Bool) -> [a] = {
  []    f -> []
  [h,t] f -> if (h.f)
    then ([h] + select t f)
    else (select t f)
}

fun qsort : [a] -> [a] = {
    []    -> []
    [h,t] -> t.select (x -> x < h : a -> Bool).qsort +
      ([h]+t).select (x -> x == h : a -> Bool) + 
      t.select (x -> x > h : a -> Bool).qsort
}

fun foldr : [a] -> (a->b->b) -> a -> b = {
  []     f a -> a
  [x,xs] f a -> f x (foldr xs f a)
}

fun sum : [a] -> a = es -> foldr es (x y -> x + y : a -> a) 0
fun product : [a] -> a = es -> foldr es (x y -> x * y : a -> a) 1

[4,1,5,3,2].qsort.reverse

foldr [1,2,3,4,5] (x y -> x + y : a -> a) 0
foldr [1,2,3,4,5] (x y -> x * y : a -> a) 1
sum [1,2,3,4,5]
product [1,2,3,4,5]

uniq (reverse (qsort [5,4,1,3,5,2,2]))

[5,4,1,3,5,2,2].qsort.reverse.uniq

fun * : (b->c) -> (a->b) -> (a->c) = f g -> (x -> x.g.f : a -> c)

(uniq * reverse * qsort) [5,4,1,3,5,2,2,6]