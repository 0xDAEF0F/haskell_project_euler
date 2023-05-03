import Data.Char (digitToInt)

isNumAKthDigitPower :: Integral p => p -> Int -> Bool
isNumAKthDigitPower k n = sum (allDigitsToTheKthPower k n) == n

allDigitsToTheKthPower :: Integral p => p -> Int -> [Int]
allDigitsToTheKthPower k n = map (^ k) (numberToDigitList n)

numberToDigitList :: Int -> [Int]
numberToDigitList n = map digitToInt $ show n
