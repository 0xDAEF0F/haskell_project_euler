mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort [] = []
mergeSort list = merge (mergeSort a) (mergeSort b)
  where
    (a, b) = splitAt (length list `div` 2) list

-- Will merge two sorted lists in order
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- Exercise 9
-- a.
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- b.
take' :: (Eq t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x : xs) = x : take' (n - 1) xs

-- c.
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_ : xs) = last' xs