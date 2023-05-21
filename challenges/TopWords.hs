module TopWords (top3) where

import Data.Char (isAlpha, toLower)
import Data.HashMap.Internal (HashMap, (!))
import qualified Data.HashMap.Internal as HM
import Data.List (sort, sortOn)
import Data.Ord (Down (..))
import Hledger.Utils (strip, words')

top3 :: String -> [String]
top3 str = map fst (take 3 $ sortOn (Down . snd) mapLst)
  where
    mapLst = HM.toList (toCountMap (toWords str) HM.empty)

toCountMap :: [String] -> HashMap String Int -> HashMap String Int
toCountMap xs hm = foldl (flip incrementCount) hm xs
  where
    incrementCount str hm = case HM.lookup str hm of
      Just num -> HM.insert str (num + 1) hm
      Nothing -> HM.insert str 1 hm

toWords :: String -> [String]
toWords [] = []
toWords str = word : toWords rest
  where
    (word, rest) = span isAllowed $ dropWhile (not . isAllowed) str

isAllowed c = isAlpha c || c == '\''

paragraph = "In a village of La Mancha, the name of which I have no desire to call to mind, there lived not long since one of those gentlemen that keep a lance in the lance-rack, an old buckler, a lean hack, and a greyhound for coursing. An olla of rather more beef than mutton, a salad on most nights, scraps on Saturdays, lentils on Fridays, and a pigeon or so extra on Sundays, made away with three-quarters of his income."
