module Fibonacci where

import Data.Array (Array, (!))
import Data.Array qualified as A

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

memoizedFibonacci :: Int -> Integer
memoizedFibonacci n = fibArr ! n
  where
    fibArr :: Array Int Integer
    fibArr = A.array (0, n) [(x, fib x) | x <- [0 .. n]]
    fib n = case n of
      0 -> 0
      1 -> 1
      n -> fibArr ! (n - 1) + fibArr ! (n - 2)
