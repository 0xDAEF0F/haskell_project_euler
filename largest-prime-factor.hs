-- Largest prime factor --
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600_851_475_143 ?

largestPrimeFactorAnswer :: Integer
largestPrimeFactorAnswer = head $ sieveOfEratosthenes [] [2 .. 600_851_475_142]

sieveOfEratosthenes :: Integral a => [a] -> [a] -> [a]
sieveOfEratosthenes primes [] = primes
sieveOfEratosthenes primes (x : xs) = sieveOfEratosthenes (x : primes) (filter (\num -> (num `mod` x) /= 0) xs)