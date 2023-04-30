module Combinations where

combs :: [a] -> Int -> [[a]]
combs _ 0 = [[]]
combs [] _ = []
combs (x : xs) k = map (x :) (combs xs (k - 1)) ++ combs xs k

subsets :: [a] -> [[a]]
subsets list = go list (length list)
  where
    go xs 1 = combs xs 1
    go xs n = combs xs n ++ go xs (n - 1)