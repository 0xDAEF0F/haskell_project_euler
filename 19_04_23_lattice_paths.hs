import Data.Array

-- Problem 15: Lattice paths

-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
-- there are exactly 6 routes to the bottom right corner.

latticePaths :: (Ord a1, Ord b, Num a2, Num a1, Num b) => (a1, b) -> (a1, b) -> a2
latticePaths (x, y) (ex, ey)
  | x == ex || y == ey = 1
  | x < ex && y < ey = latticePaths (x + 1, y) (ex, ey) + latticePaths (x, y + 1) (ex, ey)

latticePaths2 :: Int -> Int -> Integer
latticePaths2 m n = memoTable ! (m, n)
  where
    bounds = ((0, 0), (m, n))
    memoTable = listArray bounds [paths (x, y) | (x, y) <- range bounds]

    paths (x, y)
      | x == 0 || y == 0 = 1
      | otherwise = memoTable ! (x - 1, y) + memoTable ! (x, y - 1)