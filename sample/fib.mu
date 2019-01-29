fun fib : Int -> Int = {
  1 -> 1
  2 -> 2
  n -> fib (n-1) + fib (n-2)
}
fib 10
