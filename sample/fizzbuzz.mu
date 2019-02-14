fun div? : Int -> Int -> Bool = {
  a b -> (b/a)*a == b
}

fun fizzbuzz : Int -> String = {
  a |15.div? a| -> "fizzbuzz"
  a | 5.div? a| -> "buzz"
  a | 3.div? a| -> "fizz"
  a             -> a.to_s
}

fun each : [a] -> (a->b) -> [b] = {
  []     f -> []
  [e;es] f -> [f e] + each es f
}

fun range : Int -> Int -> [Int] = {
  a a       -> [a]
  a b |a>b| -> []
  a b       -> [a] + range (a+1) b
}

(range 1 15).each {
  x -> puts (fizzbuzz x)
}
