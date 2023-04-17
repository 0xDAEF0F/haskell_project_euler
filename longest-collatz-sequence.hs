import Data.Function (on)
import Data.List (maximumBy)

collatzFn :: Integral a => a -> a
collatzFn n
  | even n = n `div` 2
  | otherwise = n * 3 + 1

recursionSequence :: (t -> t) -> (t -> Bool) -> t -> [t]
recursionSequence f stopCondition x
  | stopCondition x = [x]
  | otherwise = x : recursionSequence f stopCondition (f x)

collatzSequenceForN :: Integer -> [Integer]
collatzSequenceForN = recursionSequence collatzFn (== 1)

allCollatzSequencesUnderN :: Integer -> [[Integer]]
allCollatzSequencesUnderN n = map collatzSequenceForN [n, n - 1 .. 1]

largestCollatzSequenceUnderN :: Integer -> [Integer]
largestCollatzSequenceUnderN n = maximumBy (compare `on` length) (allCollatzSequencesUnderN n)
