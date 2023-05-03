insertAt :: a -> [a] -> [[a]]
insertAt x [] = [[x]]
insertAt x (y : ys) = (x : y : ys) : map (y :) (insertAt x ys)

-- With Foldr
permutations' :: [a] -> [[a]]
permutations' = foldr (concatMap . insertAt) [[]]

-- Simple Definition
permutations'' :: [a] -> [[a]]
permutations'' [] = [[]]
permutations'' (x : xs) = concatMap (insertAt x) (permutations'' xs)