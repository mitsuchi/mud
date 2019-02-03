fun sqrt : a -> a = x -> {
  sqrt_iter 1.0 x
}

fun sqrt_iter : a -> a -> a = {
  guess x -> if good_enough? guess x
    then guess
    else sqrt_iter (improve guess x) x
}

fun improve : a -> a -> a = {
  guess x -> average guess (x/guess)
}

fun good_enough? : a -> a -> Bool = {
  guess x -> abs (guess*guess - x) < 0.0001
}

fun average : a -> a -> a = x y -> (x+y)/2.0

fun abs : a -> a = x -> if x < 0 then -x else x

puts (sqrt 2.0)
