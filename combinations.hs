combs :: [a] -> Int -> [[a]]
combs _ 0 = [[]]
combs [] _ = []
combs (x:xs) k = (map (x:) (combs xs (k - 1))) ++ combs xs k
