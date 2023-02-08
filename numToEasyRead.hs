build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

numToEasyRead a = reverse $ concat allButLastWithUnderscores ++ firstThreeDigits
  where reversedChunks = chunksOf 3 $ reverse $ show $ truncate a
        allButLastWithUnderscores = map (++ "_") $ init reversedChunks
        firstThreeDigits = last reversedChunks
