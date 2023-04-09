import Data.List (find)

-- CHALLENGE
-- Find the largest prime factor of n.

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600_851_475_143 ?

largestPrimeFactor :: Integral a => a -> Maybe a
largestPrimeFactor n = find (\prime -> n `mod` prime == 0) (reverse (primeFactors n))

primeFactors :: Integral a => a -> [a]
primeFactors n = go n 2
  where
    go num divisor
      | num == 1 = []
      | num `mod` divisor == 0 = divisor : go (num `div` divisor) divisor
      | otherwise = go num (divisor + 1)

-- Generates all prime numbers up to N using sieveOfEratosthenes
allPrimesUpToN :: Integral a => a -> [a]
allPrimesUpToN n = sieveOfEratosthenes [] [2 .. n]
  where
    sieveOfEratosthenes primes [] = reverse primes
    sieveOfEratosthenes primes (x : xs) = sieveOfEratosthenes (x : primes) (filter (\num -> (num `mod` x) /= 0) xs)
