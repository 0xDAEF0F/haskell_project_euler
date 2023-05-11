import Control.Monad (liftM2)
import Data.Ix (range)

grid =
  [ [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]

-- Helper function
sumSubgrid :: Int -> Int -> Int -> [[Int]] -> Int
sumSubgrid k row col grid = sum [grid !! (r + row) !! (c + col) | r <- [0 .. (k - 1)], c <- [0 .. (k - 1)]]

-- Simple
maxSumOfSubGrids :: [[Int]] -> Int -> Int
maxSumOfSubGrids grid k =
  maximum [sumSubgrid k row col grid | row <- [0 .. nrows - k], col <- [0 .. ncols - k]]
  where
    nrows = length grid
    ncols = length (head grid)

-- Complex
maxSumOfSubGrids' :: [[Int]] -> Int -> Int
maxSumOfSubGrids' grid k = maximum subgridSums
  where
    nrows = length grid
    ncols = length (head grid)
    validRowIndices = range (0, nrows - k)
    validColIndices = range (0, ncols - k)
    indexPairs = liftM2 (,) validRowIndices validColIndices
    subgridSums = sumSubgrid k <$> fst <*> snd <$> indexPairs <*> pure grid
