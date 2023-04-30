import PrimeNumbers (primeFactors)

-- Problem 12 Project Euler

triangularSum :: (Num a, Enum a) => a -> a
triangularSum n = sum [1 .. n]

numOfDivisors = product . map ((+ 1) . length) . group' . primeFactors

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x : xs) = go [x] xs
  where
    go currentGroup [] = [currentGroup]
    go currentGroup@(y : _) (z : zs)
      | y == z = go (z : currentGroup) zs
      | otherwise = currentGroup : go [z] zs
