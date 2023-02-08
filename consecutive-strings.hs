-- longestConsec :: [String] -> Int -> String
-- longestConsec strarr k = [words, words <- strarr]

combs' :: Int -> [a] -> [[a]]
combs' 0 _ = [[]] 
combs' _ [] = []
combs' k (x:xs) = map (x:) (combs' (k-1) xs) ++ combs' k xs