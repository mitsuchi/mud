fun add : Int -> Int -> Int = x y -> x + y
fun add : String -> String -> String = x y -> x + ' ' + y
fun add = x y -> x

puts (add 2 3) #=> 5
puts (add 'hello' 'world') #=> 'hello world'
puts (add 2.0 3.0) #=> 2.0
