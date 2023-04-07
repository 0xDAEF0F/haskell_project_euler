-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
largestPalindromeProduct :: Integer
largestPalindromeProduct = maximum $ filter withoutPalindromes $ map product (combs [100 .. 999] 2)
  where
    withoutPalindromes n = show n == reverse (show n)

combs :: [a] -> Int -> [[a]]
combs _ 0 = [[]]
combs [] _ = []
combs (x : xs) k = map (x :) (combs xs (k - 1)) ++ combs xs k