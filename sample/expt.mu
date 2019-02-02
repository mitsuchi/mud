n = 10

dc = 0

fun ** : Int -> Int = {
  b 0 -> 1
  b n -> b * ( b ** (n-1) )
}

puts (2**n)

fun expt2 : Int -> Int = {
  b n -> exptIter b n 1
}

fun exptIter : Int -> Int -> Int -> Int = {
  b 0 product -> product
  b counter product -> exptIter b (counter - 1) (b * product)
}

puts (expt2 2 n)

fun fastExpt : Int -> Int = {
  b 0 -> 1
  b n -> if even n
    then square (fastExpt b (n/2))
    else b * fastExpt b (n-1)
}

fun square : Int -> Int = n -> n * n
fun even : Int -> Bool = n -> 2*(n/2) == n

puts (fastExpt 2 n)