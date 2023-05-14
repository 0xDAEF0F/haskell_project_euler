import Data.List.Split (split, whenElt)
import Data.Char (isLetter)

abbreviate :: String -> String
abbreviate str = concat $ map shortenWord $ splittedWords str

splittedWords :: String -> [String]
splittedWords = split (whenElt (not . isLetter))

shortenWord :: String -> String
shortenWord word
  | length word <= 3 = word
  | otherwise = [head word] ++ (show $ length word - 2) ++ [last word]
