partition :: [a] -> (a -> Bool) -> ([a], [a])
partition [] _ = ([],[])
partition (x:xs) p 
    | p x = (x:ys, zs) 
    | otherwise = (ys, x:zs)
    where (ys, zs) = partition xs p

map' :: [a] -> (a -> b) -> [b]
map' [] _ = []
map' (x:xs) f = (f x):(map' xs f)

filter' :: [a] -> (a -> Bool) -> [a]
filter' [] _ = []
filter' (x:xs) p
    | p x = x:ys
    | otherwise = ys
    where ys = filter' xs p

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take (n - 1) xs 

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

chunk' :: Int -> [a] -> [[a]]
chunk' _ [] = [] 
chunk' n xs = take n xs : chunk' n (drop n xs)

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:xs) = xs

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x