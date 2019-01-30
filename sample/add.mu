fun add : Int -> Int = {
  0 n -> n
  x y -> add (x-1) (y+1)
}

puts (add 10 20)
