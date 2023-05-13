import Data.List (nub)

-- Behavior of function:
-- If the divisor divides "cleanly" into the dividend, it will return an empty list.
-- e.g., f 4 2 = []
-- If not, all of the remainders will be in the returned list.
-- e.g., f 5 4 = [2,5] || f 1 8 = [1,2,5] || f 1 11 = [0,9,0,9,..]
--               ^ 1.25           ^ 0.125             ^ 0.0909..
-- NOTE: The function will only take integers as arguments.

f :: Int -> Int -> [Int]
f a b = tail $ go a b
  where
    go dividend divisor
      | remainder == 0 = [times]
      | otherwise = times : go (remainder * 10) divisor
      where
        (times, remainder) = dividend `divMod` divisor

theCycle = cycle [0, 5, 2, 6, 3, 1, 5, 7, 8, 9, 4, 7, 3, 6, 8, 4, 2, 1]

filterOddIdx = concat . zipWith ($) (cycle [pure, const []])

filterEvenIdx = concat . zipWith ($) (cycle [const [], pure])

filterOddIdx' :: [a] -> [a]
filterOddIdx' xs = xs >>= \x -> [x | (x, i) <- zip xs [0 ..], even i]

filterOddIdx'' xs = concatMap (\x -> [x | (x, i) <- zip xs [0 ..], even i]) xs