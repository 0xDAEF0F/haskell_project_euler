module Factorial where

fact :: (Eq t, Num t) => t -> t
fact 0 = 0
fact 1 = 1
fact n = n * fact (n - 1)
