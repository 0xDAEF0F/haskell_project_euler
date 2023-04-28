-- CHALLENGE

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

tenThousandAndOnePrimeNumber :: Integer
tenThousandAndOnePrimeNumber = primeNumbers !! 10_000

primeNumbers :: [Integer]
primeNumbers = 2 : sieve [3, 5 ..]
  where
    sieve (p : rest) = p : sieve (filter (\n -> n `mod` p /= 0) rest)