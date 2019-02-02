type Rat = { n:Int, d:Int }

fun gcd : Int -> Int -> Int = {
  1 b -> 1
  a 1 -> 1
  a b -> if a == b
    then a
    else {
      aa = if a < 0 then -a else a
      bb = if b < 0 then -b else b
      if aa < bb
      then gcd aa (bb-aa)
      else gcd (aa-bb) bb
    }
}

fun mkRat : Int-> Int -> Rat = x y -> {
  d = gcd x y 
  Rat (x/d) (y/d)
}

fun + : Rat -> Rat -> Rat = x y -> {
  mkRat (x.n*y.d + x.d*y.n) (x.d * y.d)
}

fun - : Rat -> Rat -> Rat = x y -> {
  mkRat (x.n*y.d - x.d*y.n) (x.d * y.d)
}

fun * : Rat -> Rat -> Rat = x y -> {
  mkRat (x.n*y.n) (x.d*y.d)
}

x = Rat 1 2
y = Rat 2 3

puts (x + y)
puts (x * y)
puts (x - y)
