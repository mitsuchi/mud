
fun sqrt : a -> a = x -> {
  fun sqrtIter : a -> a -> a = {
    guess x -> if goodEnough guess x
      then guess
      else sqrtIter (improve guess x) x
  }
  fun improve : a -> a -> a = {
    guess x -> average guess (x/guess)
  }
  fun goodEnough : a -> a -> Bool = {
    guess x -> abs (guess*guess - x) < 0.0001
  }
  fun average : a -> a -> a = x y -> (x+y)/2.0
  fun abs : a -> a = x -> if x < 0 then -x else x
  sqrtIter 1.0 x
}

puts (sqrt 2.0)
