data Rat = R | L | P deriving (Show, Eq)

countDeafRats str = leftFacingRatsOfLeftSide + rightFacingRatsOfRightSide
  where
    (left, right) = break (== P) $ stringToRats $ filter (/=' ') str
    leftFacingRatsOfLeftSide = count (== L) left
    rightFacingRatsOfRightSide = count (== R) (drop 1 right)

stringToRats :: String -> [Rat]
stringToRats [] = []
stringToRats [x] = [P]
stringToRats (x : y : ys)
  | x == 'P' = P : stringToRats (y : ys)
  | x == 'O' = L : stringToRats ys
  | x == '~' = R : stringToRats ys

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where
    go !n [] = n
    go !n (x : xs)
      | p x = go (n + 1) xs
      | otherwise = go n xs
