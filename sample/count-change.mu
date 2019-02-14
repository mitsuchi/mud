fun firstDenomination : Int -> Int = {
  1 -> 1
  2 -> 5
  3 -> 10
  4 -> 25
  5 -> 50
}

fun cc : Int -> Int -> Int = {
  0 kindsOfCoins -> 1
  amount 0      -> 0
  amount kindsOfCoins -> if amount < 0
    then 0
    else (cc amount (kindsOfCoins - 1)) +
         (cc (amount - firstDenomination kindsOfCoins) kindsOfCoins)
}

fun countChange : Int -> Int = {
  amount -> cc amount 5
}

puts (countChange 100)