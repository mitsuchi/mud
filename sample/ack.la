fun ack : Int -> Int -> Int = {
  0 n -> n + 1
  m 0 -> ack (m-1) 1
  m n -> ack (m-1) (ack m (n-1))
}
ack 2 3
