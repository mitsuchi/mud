#!/bin/bash
try() {
  expected="$1"
  input="$2"

  actual=`echo $input | fuga-exe`

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expected, but got $actual"
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

echo OK