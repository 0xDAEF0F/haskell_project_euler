groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' _ [x] = [[x]]
groupBy' p (x:y:ys)
  | p x y = (x : head groups) : tail groups
  | otherwise = [x] : groups
  where
    groups = groupBy' p (y:ys)

span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ xs@[] = (xs, xs)
span' p xs@(x:xs')
  | p x =  let (ys,zs) = span' p xs' in (x:ys,zs)
  | otherwise = ([],xs)
