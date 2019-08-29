fun mod : Int -> Int -> Int = {
    num divBy -> {
        num - divBy * (num / divBy)
    }
}

fun uintToString : Int -> String = {
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
    over10 -> (over10 / 10).uintToString + (over10.mod 10).uintToString
}

fun toString : Int -> String = {
    num -> if num < 0 then "-" + (-num).uintToString else num.uintToString
}

fun toString : Bool -> String = {
    True  -> "True"
    False -> "False"
}

fun toString : String -> String = {
    str -> '"' + str + '"'
}

fun listToStringInternal : [a] -> String = {
    []     -> "]"
    [x;xs] -> "," + x.toString + xs.listToStringInternal
}

fun toString : [a] -> String = {
    [x;xs] -> "[" + x.toString + xs.listToStringInternal
}


puts (False.toString + " and " + -1234567.toString + " and " + [1,2,3,4].toString + " and " + "This is String".toString + " is String")
