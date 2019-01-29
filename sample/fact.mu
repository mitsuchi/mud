fun fact : Int -> Int = {
  1 -> 1
  n -> n * fact (n-1)
}
fact 10