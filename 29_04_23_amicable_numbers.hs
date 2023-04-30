import Combinations (combs)
import Data.List (nub)
import PrimeNumbers (primeFactors)

sumAllAmicableNumbersUnderTenThousand :: Integer
sumAllAmicableNumbersUnderTenThousand = sum $ filter isAmicableNumber [2 .. 9_999]

isAmicableNumber :: Integral a => a -> Bool
isAmicableNumber n = childSum == n && sumOfDivisors /= childSum
  where
    sumOfDivisors = sum $ divisorsOfNumber n
    childSum = sum $ divisorsOfNumber sumOfDivisors

divisorsOfNumber :: Integral t => t -> [t]
divisorsOfNumber = go 1
  where
    go n t
      | n == t = []
      | t `mod` n == 0 = n : go (n + 1) t
      | otherwise = go (n + 1) t
