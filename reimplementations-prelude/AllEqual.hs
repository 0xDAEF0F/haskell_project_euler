module AllEqual where

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)

allEqual :: Eq a => [a] -> Bool
allEqual = and . mapAdjacent (==)

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = splitAt (length xs `div` 2) xs