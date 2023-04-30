module PrimeNumbers where

import Data.List (find)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = x : primeFactors (n `div` x)
  where
    Just x = find (\p -> n `mod` p == 0) primeNumbers

primeNumbers :: [Integer]
primeNumbers = 2 : sieve [3, 5 ..]
  where
    sieve (p : rest) = p : sieve (filter (\n -> n `mod` p /= 0) rest)