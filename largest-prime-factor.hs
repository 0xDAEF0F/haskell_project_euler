import Data.List (find)

-- Largest prime factor --
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600_851_475_143 ?

largestPrimeFactorForN :: Integral a => a -> Maybe a
largestPrimeFactorForN n = find (\prime -> n `mod` prime == 0) (sieveOfEratosthenes [] [2 .. n])

sieveOfEratosthenes :: Integral a => [a] -> [a] -> [a]
sieveOfEratosthenes primes [] = primes
sieveOfEratosthenes primes (x : xs) = sieveOfEratosthenes (x : primes) (filter (\num -> (num `mod` x) /= 0) xs)