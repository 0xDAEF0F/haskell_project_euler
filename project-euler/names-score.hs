import Data.Char (ord)
import Data.List (sort)

-- Problem 22 Project Euler

main :: IO ()
main = do
  let fileName = ""
  contents <- readFile fileName
  let totalSum = sum $ zipWith (curry nameWithOrderingToScore) (sort $ words $ cleanse contents) [1 ..]
  print totalSum

cleanse :: String -> String
cleanse = map (\c -> if c == ',' then ' ' else c) . filter (/= '"')

nameWithOrderingToScore :: (String, Int) -> Int
nameWithOrderingToScore (name, pos) = nameToScore name * pos

nameToScore :: String -> Int
nameToScore = sum . map (\c -> ord c - 64)
