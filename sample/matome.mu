# hello, world!
puts "hello, world!"

# 算術
1+2      #=> 3
1+2*3    #=> 7
(1+2)*3  #=> 9
  
# 関数定義：1変数
fun double : Int -> Int = x -> x * 2

# 関数適用
double 10  #=> 20

# 関数定義；2変数
fun add : Int -> Int -> Int = x y -> x + y

# 関数適用
add 10 20  #=> 30

# . 演算子は、関数適用の関数と第一引数をひっくりかえす
10.double  #=> double 10 と等価、つまり 20
10.add 20  #=> add 10 20 と等価、つまり 30
10.add 20.double  #=> double (add 10 20) と等価、つまり 60
  
# 変数
a=10
b=20
a+b  #=> 30

# パターンマッチ、再帰的定義
# 1 + 2 + .. + n を計算する関数
fun sum : Int -> Int = {
    1 -> 1
    n -> n + sum (n-1)
}
sum 5   #=> 15

# 1 * 2 * .. * n を計算する関数
fun factorial : Int -> Int = {
    1 -> 1
    n -> n * (factorial (n-1))
}
factorial 5   #=> 120

# リスト
[1,2,3] + [4,5]   #=> [1,2,3,4,5]

# リスト上の写像
fun map : [a] -> (a->b) -> [b] = {
  []     f -> []
  [e;es] f -> [f e] + map es f
}

map [1,2,3] double  #=> [2,4,6]
[1,2,3].map double  #=> [2,4,6] 

# 同じ関数名でも引数の型によって異なる関数が呼び出される
fun incr : Int -> Int = x -> x + 1
fun incr : String -> String = {
  'zero' -> 'one'
  'one' -> 'two'
  'two' -> 'three'
}

incr 2       #=> 3
incr 'two'   #=> 'three'

# 多重ディスパッチ
# （＝引数が複数ある場合、すべての引数の型の組み合わせごとに異なる関数が定義できる）
# ベクトルの定数倍
fun * : [Int] -> Int -> [Int] = {
  xs y -> xs.map (a -> a * y)
}
[1,2,3] * 3    #=> [3,6,9]

# ベクトルの内積
fun * : [Int] -> [Int] -> Int = {
  xs []  -> 0
  [] ys  -> 0
  [x;xs] [y;ys] -> x * y + xs * ys
}
[1,2,3] * [4,5,6]    #=> 32

# 匿名関数
(x -> x + 1 : Int -> Int) 1  #=> 2
[1,2,3].map (x -> x * 4 : Int -> Int)  #=> [4,8,12]
  
# リストのソート
fun select : [a] -> (a -> Bool) -> [a] = {
  []     f -> []
  [e;es] f -> if (e.f)
    then ([e] + select es f)
    else (select es f)
}

fun qsort : [a] -> [a] = {
    []    -> []
    [e;es] -> es.select (x -> x < e : a -> Bool).qsort +
      ([e]+es).select (x -> x == e : a -> Bool) + 
      es.select (x -> x > e : a -> Bool).qsort
}

[2,5,1,2,4,3].qsort    #=> [1,2,2,3,4,5]

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

# パラメトリック多相
fun id : a -> a = x -> x
id 1     #=> 1
id 'one' #=> "one"

# 関数に引数を適用するだけの関数 apply
fun apply : (a->a) -> a -> a = f x -> f x
apply inc 0 #=> 1

# 関数名の末尾に ? を使う
fun even? : Int -> Bool = x -> x/2*2 == x
even? 8 #=> True
even? 9 #=> False

# 関数名、変数名の途中と末尾に _ を使う
fun to_s : Complex -> String = {
  c -> c.r.to_s + "+" + c.i.to_s + "i"
}
(Complex 1 2).to_s #=> "1+2i"

# 関数名、変数名の末尾に ' を使う
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
fun divide? : Int -> Int -> Bool = {
  a b -> (b/a)*a == b
}
fun fizzbuzz : Int -> String = {
  a |15.divide? a| -> "fizzbuzz"
  a | 5.divide? a| -> "buzz"
  a | 3.divide? a| -> "fizz"
  a                -> a.to_s
}

# 匿名関数の型を省略する。
# 以下の場合 (x -> x + x : a -> b) のように最も一般的な型とみなされる。
[1,2,3].map (x -> x + x)   #=> [2,4,6]

# 通常の関数の型を省略する。
# 同様に triple : a -> b とみなされる。
fun triple = x -> x * 3
triple 10 #=> 30
triple "a" #=> "aaa"

# なるべく楽して関数を定義する
double' = x -> x + x
double' 10    #=> 20

double'' = x -> x + " " + x : String -> String
double'' "hoge"   #=> "hoge hoge"
