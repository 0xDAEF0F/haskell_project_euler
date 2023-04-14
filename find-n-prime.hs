-- CHALLENGE
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

tenThousandAndOnePrimeNumber :: Integer
tenThousandAndOnePrimeNumber = last $ take 10_001 allPrimeNumbers

nPrimeNumber :: Int -> Integer
nPrimeNumber n = last $ take n allPrimeNumbers

allPrimeNumbers :: [Integer]
allPrimeNumbers = sieve [2 ..]
  where
    sieve (x : xs) = x : sieve (filter (\num -> (num `mod` x) /= 0) xs)

-- Option 2
primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\p -> n `mod` p /= 0) . takeWhile (\p -> p * p <= n) $ primes
