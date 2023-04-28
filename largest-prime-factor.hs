import Data.List (find)

-- CHALLENGE
-- Find the largest prime factor of n.

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600_851_475_143 ?

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = last $ primeFactors n

primeNumbers :: [Integer]
primeNumbers = 2 : sieve [3, 5 ..]
  where
    sieve (p : rest) = p : sieve (filter (\n -> n `mod` p /= 0) rest)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = case find (\p -> n `mod` p == 0) primeNumbers of
  Nothing -> []
  Just x -> x : primeFactors (n `div` x)