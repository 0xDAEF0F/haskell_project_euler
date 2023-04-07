ssd n = squareOfSums - sumOfSquares
    where sumOfSquares = sum $ map (\x -> x ^ 2) [1..n]
          squareOfSums = sum [1..n] ^ 2 
