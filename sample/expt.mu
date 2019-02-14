n = 10

dc = 0

fun ** : Int -> Int -> Int = {
  b 0 -> 1
  b n -> b * ( b ** (n-1) )
}

puts (2**n)

fun expt_iter : Int -> Int -> Int -> Int = {
  b 0 product -> product
  b counter product -> expt_iter b (counter - 1) (b * product)
}

fun expt2 : Int -> Int -> Int = {
  b n -> expt_iter b n 1
}

puts (expt2 2 n)

fun square : Int -> Int = n -> n * n
fun even : Int -> Bool = n -> 2*(n/2) == n

fun fast_expt : Int -> Int -> Int = {
  b 0 -> 1
  b n -> if even n
    then square (fast_expt b (n/2))
    else b * fast_expt b (n-1)
}

puts (fast_expt 2 n)