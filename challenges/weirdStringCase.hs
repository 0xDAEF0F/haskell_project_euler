import Data.Char (toUpper, toLower)

weirdStringCase :: String -> String
weirdStringCase str = unwords (map (zipWith ($) weird) (words str))

weird = cycle [toUpper, toLower]

-- words :: String -> [String]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- map :: (a -> b) -> [a] -> [b]
-- unwords :: [String] -> String
-- cycle :: [a] -> [a]
-- weird :: [Char -> Char] list of functions from char to char

-- How do you map through 
