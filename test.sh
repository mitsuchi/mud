#!/bin/bash
try() {
  expected="$1"
  input="$2"

  actual=`echo "$input" | fuga-exe`

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expected, but got $actual, input = $input"
    exit 1
  fi
}

try 0 0
try 42 42
try 3 "1+2"
try 7 "1+2*3"
try 9 "(1+2)*3"
try 2 "3-1"
try 1 "a=1;a"
try 3 "a=1;b=2;a+b"
try 120 "fun fact : Int -> Int = {1->1; n->n*fact(n-1)}; fact 5"
try 11 "fun inc : Int -> Int = n -> n + 1; 10.inc"
try 3 "fun add : Int -> Int -> Int = x y -> x + y; add 1 2"
try 3 "fun add : Int -> Int -> Int = x y -> x + y; 1.add 2"
try 6 "fun add : Int -> Int -> Int = x y -> x + y; 1.add 2.add 3"
try "hello" "'hello'"
try "hello world" "'hello'+' world'"
try "hellohellohello" "'hello'*3"
try 3 "fun add : Int -> Int -> Int = x y -> x + y; fun add : String -> String -> String = x y -> x + y; 1+2"
try "ab" "fun add : Int -> Int -> Int = x y -> x + y; fun add : String -> String -> String = x y -> x + y; 'a'+'b'"
try 40 "fun double : Int -> Int = x -> x + x; fun twice : (Int->Int)->Int->Int = f x -> f (f x); twice (double:Int->Int) 10"
try "a a a a" "fun double : Int -> Int = x -> x + x; fun double : String -> String = x ->x + ' ' + x; fun twice : (String->String)->String->String = f x -> f (f x); twice (double:String->String) 'a'"
try "10" "fun id : a -> a = x -> x; id 10;"
try "hoge" "fun id : a -> a = x -> x; id 'hoge';"
try 40 "fun double : Int -> Int = x -> x + x; fun twice : (a->a)->a->a = f x -> f (f x); twice (double:Int->Int) 10"
try 40 "fun double : Int -> Int = x -> x + x; fun twice : (Int->Int)->Int->Int = f x -> f (f x); twice double 10"
try 10 "fun id : a -> a = x -> x; (id id) 10"
try "aaaa" "fun double : a -> a = x -> x + x; fun twice : (a->a)->a->a = f x -> f (f x); twice double 'a'"
try 40 "fun comp : (b->c) -> (a->b) -> a -> c = f g x -> f (g x); fun double : a->a = x -> x + x; comp double double 10"
try 20 "(x->x+x:Int->Int) 10"
try "aa" "(x->x+x:String->String) 'a'"
try "40" "(x->x+x:a->a) 20"
try "hogehoge" "(x->x+x:a->a) 'hoge'"
try "40" "fun twice : (a->a)->a = f x -> f (f x); twice (x->x+x:a->a) 10"
try "bbbb" "fun twice : (a->a)->a = f x -> f (f x); twice (x->x+x:a->a) 'b'"
try 20 "fun apply : (a->a)->(a->a) = f -> (x -> f x : a->a); fun double:a->a = x->x+x; (apply double) 10"
try 40 "fun twice : (a->a)->(a->a) = f -> (x -> f (f x) : a->a); fun double:a->a = x->x+x; (twice double) 10"
try 1024 "fun ** : Int -> Int -> Int = { a 0 -> 1; a b -> (a ** (b-1)) * a }; 2 ** 10"
try 3 "fun ** : (b->c) -> (a->b) -> (a->c) = f g -> (x -> x.g.f : a -> c); fun inc : Int -> Int = x -> x + 1; fun double : Int -> Int = x -> x * 2; (inc ** double) 1"
try 5 "fun length : [a] -> Int = { [] -> 0; es -> 1 + length (tail es) }; length [4,5,6,7,8]"
try "[2,4,6]" "fun map : [a] -> (a->a) -> [a] = { [] f -> []; es f -> [f (head es)] + (map (tail es) f)}; map [1,2,3] (x -> x + x : Int -> Int)"
try "[4,3,2,1]" "fun reverse : [a] -> [a] = { [] -> []; es -> (reverse (tail es)) + [head es] }; reverse [1,2,3,4]"
try "True" "True && True"
try "False" "True && False"
try "True" "True || True"
try "True" "True || False"
try "True" "True + True"
try "False" "True * False"
try "True" "1 < 2"
try "False" "1 == 2"
try "True" "1 == 1"
try "True" "[1,2] == [1,2]"
try "False" "1 > 2"
try "False" "(1 > 2) && (3 == 3)"
try "2" "if 1==1 then 2 else 3"
try "3" "if 1>1 then 2 else 3"

echo OK