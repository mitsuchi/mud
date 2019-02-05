fun length : [a] -> Int = {
  []    -> 0
  [e;es] -> 1 + length es
}

fun map : [a] -> (a->b) -> [b] = {
  []    f -> []
  [e;es] f -> [f e] + map es f
}

fun reverse : [a] -> [a] = {
  []    -> []
  [e;es] -> reverse es + [e]
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
  [e;es] f -> if (e.f)
    then ([e] + select es f)
    else (select es f)
}

fun qsort : [a] -> [a] = {
    []    -> []
    [e;es] -> es.select (x -> x < e : a -> Bool).qsort +
      ([e]+es).select (x -> x == e : a -> Bool) + 
      es.select (x -> x > e : a -> Bool).qsort
}

fun foldr : [a] -> (a->b->b) -> a -> b = {
  []     f a -> a
  [x;xs] f a -> f x (foldr xs f a)
}

fun sum : [a] -> a = es -> foldr es (x y -> x + y : a -> a) 0
fun product : [a] -> a = es -> foldr es (x y -> x * y : a -> a) 1

[4,1,5,3,2].qsort.reverse.puts

puts (foldr [1,2,3,4,5] (x y -> x + y : a -> a) 0)
puts (foldr [1,2,3,4,5] (x y -> x * y : a -> a) 1)
puts (sum [1,2,3,4,5])
puts (product [1,2,3,4,5])

puts (uniq (reverse (qsort [5,4,1,3,5,2,2])))

[5,4,1,3,5,2,2].qsort.reverse.uniq.puts

fun * : (b->c) -> (a->b) -> (a->c) = f g -> (x -> x.g.f : a -> c)

puts ((uniq * reverse * qsort) [5,4,1,3,5,2,2,6])