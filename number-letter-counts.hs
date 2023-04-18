-- Problem 17

-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

units :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

numToWord :: Int -> [Char]
numToWord n
  | n < 10 = units !! n
  | n < 20 = teens !! (n - 10)
  | n < 100 = tens !! ((n `div` 10) - 2) ++ if n `mod` 10 == 0 then "" else "-" ++ units !! (n `mod` 10)
  | n < 1000 = units !! (n `div` 100) ++ " hundred" ++ if n `mod` 100 == 0 then "" else " and " ++ numToWord (n `mod` 100)
  | n == 1000 = "one thousand"

answer :: Int
answer = sum $ map (length . filter (\c -> c /= '-' && c /= ' ') . numToWord) [1 .. 1000]