import Data.List (find)
import PrimeNumbers (primeFactors, primeNumbers)

-- CHALLENGE
-- Find the largest prime factor of n.

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600_851_475_143 ?

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = last $ primeFactors n
