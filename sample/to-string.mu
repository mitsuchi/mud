fun mod : Int -> Int -> Int = {
    num divBy -> {
        num - divBy * (num / divBy)
    }
}

fun toString : Int -> String = {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    over10 -> (over10 / 10).toString + (over10.mod 10).toString
}

fun toString : Bool -> String = {
    True  -> "True"
    False -> "False"
}

puts (False.toString + " and " + 1234567890.toString + " is String")
