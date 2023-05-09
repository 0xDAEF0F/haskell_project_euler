import Data.List (nub)

-- Behavior of function:
-- If the divisor divides "cleanly" into the dividend, it will return an empty list.
-- e.g., f 4 2 = []
-- If not, all of the remainders will be in the returned list.
-- e.g., f 5 4 = [2,5] || f 1 8 = [1,2,5] || f 1 11 = [0,9,0,9,..]
--               ^ 1.25           ^ 0.125             ^ 0.0909..
-- NOTE: The function will only take integers as arguments.

f :: Int -> Int -> [Int]
f dividend divisor
  | dividend `mod` divisor == 0 = []
  | otherwise = undefined

-- IDEAS
-- nub . map snd $ iterate (\(a, _) -> divMod (10 * a) divisor) (divMod (10 * dividend) divisor)
-- takeWhile (/= 0) $ iterate (\x -> (x * 10) `mod` divisor) (dividend `mod` divisor)