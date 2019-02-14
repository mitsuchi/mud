fun remainder : Int -> Int -> Int = {
    a b -> a - (a / b) * b
}

fun devides? : Int -> Int -> Bool = {
    a b -> 0 == remainder b a 
}

fun square : Int -> Int = {
    n -> n * n
}

fun find_devisor : Int -> Int -> Int = n test_devisor -> {
    if square test_devisor > n 
      then n
      else if devides? test_devisor n
        then test_devisor
        else find_devisor n (test_devisor+1)
}

fun smallest_devisor : Int -> Int = n -> {
    find_devisor n 2
}

fun prime? : Int -> Bool = {
    n -> n == smallest_devisor n
}

puts (prime? 3539)