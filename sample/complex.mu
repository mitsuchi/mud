type Complex = { r:Int, i:Int }

fun + : Complex -> Complex -> Complex = x y -> Complex (x.r+y.r) (x.i+y.i)
fun - : Complex -> Complex -> Complex = x y -> Complex (x.r-y.r) (x.i-y.i)
fun * : Complex -> Complex -> Complex = x y -> Complex (x.r*y.r-x.i*y.i) (x.r*y.i+x.i*y.r)

Complex 1 2 + Complex 3 4 * Complex 5 6
