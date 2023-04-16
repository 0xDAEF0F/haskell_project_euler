-- Problem 12 Project Euler

triangularSum :: (Num a, Enum a) => a -> a
triangularSum n = sum [1 .. n]

primeFactors :: Integral a => a -> [a]
primeFactors n = go n 2
  where
    go num divisor
      | num == 1 = []
      | num `mod` divisor == 0 = divisor : go (num `div` divisor) divisor
      | otherwise = go num (divisor + 1)

numOfDivisors = product . map ((+ 1) . length) . group' . primeFactors

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x : xs) = go [x] xs
  where
    go currentGroup [] = [currentGroup]
    go currentGroup@(y : _) (z : zs)
      | y == z = go (z : currentGroup) zs
      | otherwise = currentGroup : go [z] zs
