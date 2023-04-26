module Validate where

import Data.Char (digitToInt, intToDigit)

validate num
  | even (length $ show num) = isModTenZero $ sum' $ zipWith ($) (cycle [magic, id]) (show num)
  | otherwise = isModTenZero $ sum' $ zipWith ($) (cycle [id, magic]) (show num)
  where
    sum' = foldr (\a b -> digitToInt a + b) 0
    isModTenZero n = n `mod` 10 == 0
    magic = intToDigit . (\n -> if n > 9 then n - 9 else n) . (* 2) . digitToInt
