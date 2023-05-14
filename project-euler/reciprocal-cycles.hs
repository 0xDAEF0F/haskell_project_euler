import Data.List (nub, find, maximumBy)
import Data.Maybe (mapMaybe)
import Data.Function (on)

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

foo cycle = case find (\(t, h, i) -> t == h && i > 1) bar 
  of
    Nothing -> Nothing
    Just (a,b,c) -> Just (take (c+1) cycle)
  where
    bar = zip3 cycle baz [0..]
    baz = [x | (x, i) <- zip cycle [0..], odd i]

cycleExample = cycle [0, 5, 2, 6, 3, 1, 5, 7, 8, 9, 4, 7, 3, 6, 8, 4, 2, 1]

-- fn (_,_,i) (_,_,i2) = i > i2 

bar = mapMaybe (foo . f 1) [2..999]
