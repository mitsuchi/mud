fun sum : (a->a)->a->(a->a)->a->a = {
  term a next b |a>b| -> 0
  term a next b       -> term a + sum term (next a) next b
}

fun inc : a -> a = n -> n + 1
fun cube : a -> a = n -> n * n * n

fun sum_cubes : a -> a -> a = {
  a b -> sum cube a inc b
}

puts (sum_cubes 1 10)

fun identity : a -> a = x -> x

fun sum_integers : a -> a -> a = {
  a b -> sum identity a inc b
}

puts (sum_integers 1 10)

fun pi_sum : a -> a -> a = a b -> {
  fun pi_term : a -> a = x -> 1.0 / (x*(x+2))
  fun pi_next : a -> a = x -> x + 4
  sum pi_term a pi_next b
}

puts (8 * (pi_sum 1 100))

fun pi_sum' : a -> a -> a = a b -> {
  sum (x -> 1.0 / (x*(x+2)) : a -> a) a (x -> x + 4 : a -> a) b
}

puts (8 * (pi_sum' 1 100))