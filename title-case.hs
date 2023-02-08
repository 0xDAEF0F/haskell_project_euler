import Data.Char (toLower, toUpper)

-- 1. Make every word for both arguments sentences' lowercase --> "in the" "in the magic world"
-- 2. UpperFirst both arguments --> "In The" "In The Magic World"
-- 3. Lower case words for snd which appear in fst --> "In The" "in the Magic World"
-- 4. Uppercase first word --> "In the Magic World"

-- String
lowerCaseString :: String -> String
lowerCaseString = map toLower

upperFirst :: String -> String
upperFirst str = toUpper (head str) : drop 1 str

-- (Step 1)
lowerCaseSentence:: String -> String
lowerCaseSentence sentence = unwords $ map lowerCaseString $ words sentence

-- (Step 2)
upperFirstSentence:: String -> [String]
upperFirstSentence sentence =  map upperFirst $ words sentence

-- (Step 3)
titleCase :: String -> String -> String
titleCase a b = 
    let exceptions = upperFirstSentence $ map toLower a
        title = upperFirstSentence $ map toLower b
    in upperFirst $ unwords $ map (\w -> if w `elem` exceptions then lowerCaseString w else w) title