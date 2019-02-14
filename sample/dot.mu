fun inc : Int -> Int = x -> x + 1
puts (10.inc)

fun add : Int -> Int -> Int = x y -> x + y
puts (1.add 2)
puts (1.add 2.add 3)