-- Problem 11
-- Largest Product in a Grid

-- In the 20x20 grid below, four numbers along a diagonal line have been marked in red.
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally)
-- in the 20x20 grid?

matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]

rightList (x : y : ys) = (x, y) : rightList (y : ys)
rightList _ = []