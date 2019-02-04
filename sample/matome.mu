# 算術
1+2      #=> 3
1+2*3    #=> 7
(1+2)*3  #=> 9
  
# 関数定義：1変数
fun double : Int -> Int = x -> x * 2

# 関数定義；2変数
fun add : Int -> Int -> Int = x y -> x + y

# 関数適用
double 10  #=> 20
add 10 20  #=> 30

# . 演算子は、関数適用の関数と第一引数をひっくりかえす
10.double  #=> 20
10.add 20  #=> 30
10.add 20.double  #=> 60
  
# 変数
a=10
b=20
a+b  #=> 30

# 関数定義：2変数
fun plus : Int -> Int -> Int = x y -> x + y
plus 2 3  #=> 5

# パターンマッチ
fun sum : Int -> Int = {
    1 -> 1
    n -> n + sum (n-1)
}
sum 5 #=> 15

# 再帰的定義
fun factorial : Int -> Int = {
    1 -> 1
    n -> n * (factorial (n-1))
}
factorial 5  #=> 120

fun incr : Int -> Int = x -> x + 1
fun incr : String -> String = x -> x + ' one'

# 同じ関数名でも引数の型によって異なる関数が呼び出される
incr 2       #=> 3
incr 'hello' #=> "hello one"

# . 演算子を使うとオブジェクト指向のメソッド呼び出しのように見える
2.incr       #=> 3

# 配列
fun map : [a] -> (a->b) -> [b] = {
  []    f -> []
  [e,es] f -> [f e] + map es f
}

map [1,2,3] double  #=> [2,4,6]
[1,2,3].map double  #=> [2,4,6]    

# 匿名関数
(x -> x + 1 : Int -> Int) 1  #=> 2
[1,2,3].map (x -> x * 4 : Int -> Int)  #=> [4,8,12]
  
# 新しい演算子を定義
fun ** : Int -> Int -> Int = {
    a 0 -> 1
    a b -> (a ** (b-1)) * a
}
2 ** 10  #=> 1024

# 新しい型を定義
type Complex = { r:Int, i:Int }

fun + : Complex -> Complex -> Complex = a b -> Complex (a.r+b.r) (a.i+b.i)
fun - : Complex -> Complex -> Complex = a b -> Complex (a.r-b.r) (a.i-b.i)
fun * : Complex -> Complex -> Complex = a b -> Complex (a.r*b.r-a.i*b.i) (a.r*b.i+a.i*b.r)
Complex 1 2 + Complex 3 4  #=> Complex 4 6

# if文
if 1 == 2 then 3 else 4            #=> 4
if 1 == 1 && 2 == 3 then 4 else 5  #=> 5
if True then 1 else 2              #=> 1
if False then 1 else 2             #=> 2

# 高階関数
fun twice : (Int->Int) -> (Int->Int) = f -> (x -> f (f x) : Int->Int)
fun inc : Int -> Int = x -> x + 1
(twice (twice (twice inc))) 0  #=> 8
(inc.twice.twice.twice) 0      #=> 8

# 関数合成（ユーザー定義）
fun * : (b->c) -> (a->b) -> (a->c) = f g -> (x -> x.g.f : a -> c)
(double * double * inc) 10 #=> 44
10.inc.double.double       #=> 44

fun select : [a] -> (a -> Bool) -> [a] = {
  []     f -> []
  [e,es] f -> if (e.f)
    then ([e] + select es f)
    else (select es f)
}

fun qsort : [a] -> [a] = {
    []    -> []
    [e,es] -> es.select (x -> x < e : a -> Bool).qsort +
      ([e]+es).select (x -> x == e : a -> Bool) + 
      es.select (x -> x > e : a -> Bool).qsort
}

[2,5,1,2,4,3].qsort

# パラメトリック多相
fun id : a -> a = x -> x
id 1     #=> 1
id 'one' #=> "one"

# 関数に引数を適用するだけの関数 apply
fun apply : (a->a) -> a -> a = f x -> f x
apply inc 0 #=> 1

# 関数名の末尾に ? を使える
fun even? : Int -> Bool = x -> x/2*2 == x
even? 8 #=> True
even? 9 #=> False

# 関数名、変数名の途中と末尾に _ を使える
fun to_s : Complex -> String = {
  c -> c.r.to_s + "+" + c.i.to_s + "i"
}
(Complex 1 2).to_s #=> "1+2i"

# 関数名、変数名の末尾に ' を使える
a' = a + 2 #=> 3

# 多相型を含む同名の関数が複数ある場合、より具体的なほうがマッチする
fun add' : String -> String -> String = x y -> x + " " + y
fun add' : a -> a -> a = x y -> x + y

add' "hello" "world" #=> "hello world"
add' 1 2 #=> 3

# パターンマッチに条件をつける
fun abs : Int -> Int = {
  a |a<0| -> -a
  a       -> a
}
abs (-3)  #=> 3
abs 3     #=> 3

# 条件つきパターンマッチを使った fizzbuzz の例
fun fizzbuzz : Int -> String = {
  a |15.div? a| -> "fizzbuzz"
  a | 5.div? a| -> "buzz"
  a | 3.div? a| -> "fizz"
  a             -> a.to_s
}
fun div? : Int -> Int -> Bool = {
  a b -> (b/a)*a == b
}

# 匿名関数の型を省略する。
# 以下の場合 (x -> x + x : a -> b) のように最も一般的な型とみなされる。
[1,2,3].map (x -> x + x)   #=> [2,4,6]

# 通常の関数の型を省略する。
# 以下の場合 triple : a -> b とみなされる。
fun triple = x -> x * 3
triple 10 #=> 30
triple "a" #=> "aaa"
