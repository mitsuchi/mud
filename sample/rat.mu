type Rat = { n:Int, d:Int }

fun abs : Int -> Int = {
  a -> if a < 0 then -a else a
}

fun gcd : Int -> Int -> Int = {
  1 b -> 1
  a 1 -> 1
  a a -> a
  a b -> {
    a' = abs(a)
    b' = abs(b)
    if a' < b'
      then gcd a' (b'-a')
      else gcd (a'-b') b'
  }
}

fun mkRat : Int-> Int -> Rat = x y -> {
  d = gcd x y 
  Rat (x/d) (y/d)
}

fun + : Rat -> Rat -> Rat = {
  x y -> mkRat (x.n*y.d + x.d*y.n) (x.d * y.d)
}

fun - : Rat -> Rat -> Rat = x y -> {
  mkRat (x.n*y.d - x.d*y.n) (x.d * y.d)
}

fun * : Rat -> Rat -> Rat = x y -> {
  mkRat (x.n*y.n) (x.d*y.d)
}

fun to_s : Rat -> String = {
  x -> x.n.to_s + "/" + x.d.to_s
}

x = Rat 1 2
y = Rat 2 3

(x + y).to_s.puts
(x * y).to_s.puts
(x - y).to_s.puts
