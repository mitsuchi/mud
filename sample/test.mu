fun twice : (Int->Int) -> (Int->Int) = f -> x -> f (f x)
fun double : Int -> Int = x -> x + x

10.(twice double)