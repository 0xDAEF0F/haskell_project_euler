import Data.List (find, group)

-- Perfect Power

-- A perfect power is a classification of positive integers:

-- In mathematics, a perfect power is a positive integer that can be expressed
-- as an integer power of another positive integer.
-- More formally, n is a perfect power if there exist natural numbers m > 1, and k > 1 such that mk = n.

-- Example: input = 81, output = 3^4 OR 9^2

-- The problem reduces to finding the prime factors of n.
-- 4 => [2, 2] => 2^2
-- 5 => [5] => Not a perfect power because exponent must be greater than 1
-- 6 => [2, 3] => Not a perfect power
-- 9 => [3, 3] => 3^2

isPP :: Integer -> Maybe (Integer, Int)
isPP 1 = Nothing
isPP n
  | gcdOfExponents > 1 = Just (base, gcdOfExponents)
  | otherwise = Nothing
  where
    base = product $ map (\g -> head g ^ (length g `div` gcdOfExponents)) pfGroups
    pfGroups = group $ primeFactors n
    gcdOfExponents = foldr1 gcd (map length pfGroups)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = x : primeFactors (n `div` x)
  where
    Just x = find (\p -> n `mod` p == 0) primeNumbers

primeNumbers :: [Integer]
primeNumbers = 2 : sieve [3, 5 ..]
  where
    sieve (p : rest) = p : sieve (filter (\n -> n `mod` p /= 0) rest)

nums = [4, 8, 9, 16, 25, 27, 32, 36, 49, 64, 81, 100, 121, 125, 128, 144, 169, 196, 216, 225, 243, 256, 289, 324, 343, 361, 400, 441, 484]
