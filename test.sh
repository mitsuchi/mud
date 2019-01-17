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
try 1 "{a=1;a}"
try 3 "{a=1;b=2;a+b}"
try 120 "{fun fact = {1->1; n->n*fact(n-1)}; fact 5}"
try 11 "{fun inc = n -> n + 1; 10.inc}"
try 3 "{fun add = x y -> x + y; add 1 2}"
try 3 "{fun add = x y -> x + y; 1.add 2}"
try 6 "{fun add = x y -> x + y; 1.add 2.add 3}"
try "hello" "'hello'"
try "hello world" "'hello'+' world'"
try "hellohellohello" "'hello'*3"

echo OK