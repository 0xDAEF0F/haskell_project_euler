ssd n = squareOfSums - sumOfSquares
  where
    sumOfSquares = sum $ map (^ 2) [1 .. n]
    squareOfSums = sum [1 .. n] ^ 2
