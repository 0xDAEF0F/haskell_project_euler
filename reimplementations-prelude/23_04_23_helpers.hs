all' :: (t -> Bool) -> [t] -> Bool
all' _ [] = True
all' f (x : xs)
  | f x = all' f xs
  | otherwise = False

any' :: (t -> Bool) -> [t] -> Bool
any' _ [] = False
any' f (x : xs)
  | f x = True
  | otherwise = any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f list@(x : xs)
  | f x = dropWhile' f xs
  | otherwise = list

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

length' :: [a] -> Integer
length' = foldr' (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = foldr (\a b -> b ++ [a]) []

sum' :: Num a => [a] -> a
sum' = sum'' 0
  where
    sum'' v [] = v
    sum'' v (x : xs) = sum'' (v + x) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x : xs) = foldl' f (f v x) xs