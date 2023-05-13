import Data.List.Split (chunksOf) 

-- Solution 1
splitStrings :: String -> [String]
splitStrings str
  | even $ length str = inChunks
  | otherwise = init inChunks ++ [last inChunks ++ "_"]
  where
    inChunks = chunksOf 2 str

-- Solution 2
splitStrings' :: String -> [String]
splitStrings' [] = []
splitStrings' [x] = [[x, '_']]
splitStrings' (x:y:ys) = [x, y] : splitStrings' ys
