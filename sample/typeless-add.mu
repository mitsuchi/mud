fun add = x y -> x + y
puts (add 1 2)
puts (add 'a' 'b')

fun add : String -> String -> String = x y -> x + " " + y
puts (add 'hello' 'world')
