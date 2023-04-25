-- 1.
myMapFilter :: (a1 -> a2) -> (a2 -> Bool) -> [a1] -> [a2]
myMapFilter f p = filter p . map f

-- 2.
-- a.
all' :: (t -> Bool) -> [t] -> Bool
all' f [] = True
all' f (x : xs) = f x && all' f xs

all'' :: Foldable t => (a -> Bool) -> t a -> Bool
all'' f = foldr ((&&) . f) True

-- b.
any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' f = foldr ((||) . f) False

-- c.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- 3.
map' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map' f = foldr (\a b -> f a : b) []

filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' p = foldr (\a b -> if p a then (a : b) else b) []

-- 4.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x : y : ys) = f1 x : f2 y : altMap f1 f2 ys
altMap f1 f2 (x : xs) = f1 x : altMap f1 f2 xs

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' f1 f2 = zipWith ($) (cycle [f1, f2])