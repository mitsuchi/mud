fun sqrt : a -> a = x -> {
  fun sqrtIter : a -> a = {
    guess -> if goodEnough guess
      then guess
      else sqrtIter (improve guess x) x
  }
  fun improve : a -> a = {
    guess -> average guess (x/guess)
  }
  fun goodEnough : a -> Bool = {
    guess -> abs (guess*guess - x) < 0.0001
  }
  fun average : a -> a -> a = x y -> (x+y)/2.0
  fun abs : a -> a = x -> if x < 0 then -x else x
  sqrtIter 1.0 x
}

sqrt 2.0
