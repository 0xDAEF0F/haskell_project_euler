myMaximum :: Ord t => [t] -> t
myMaximum (x : xs) = go x xs
  where
    go max rest
      | null rest = max
      | y > max = go y ys
      | otherwise = go max ys
      where
        (y : ys) = rest