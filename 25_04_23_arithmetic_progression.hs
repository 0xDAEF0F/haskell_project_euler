findMissing list = getMissing (findStep' list) list

getMissing step (x : y : ys)
  | (+) x step == y = getMissing step (y : ys)
  | otherwise = x + step

findStep' (x : y : z : rest)
  | abs a < abs b = a
  | otherwise = b
  where
    a = y - x
    b = z - y