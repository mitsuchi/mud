fun fibIter : Int -> Int -> Int -> Int = {
  a b 0     -> b
  a b count -> fibIter (a+count) a (count-1)
}

fun fib : Int -> Int = {
  n -> fibIter 1 0 n
}

puts (fib 100)
