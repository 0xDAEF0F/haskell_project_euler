-- Special Pythagorean triplet
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- `a^2 + b^2 = c^2`

-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc

-- input: 12
-- output:
-- 1, 2, 9
-- 1, 3, 8
-- 1, 4, 7
-- 1, 5, 6
-- 2, 3, 7
-- 3, 4, 5

-- Option 1 (Less efficient)
findTriplets :: Integral c => c -> [(c, c, c)]
findTriplets n =
  [ (a, b, c) | a <- [1 .. n `div` 2], b <- [a + 1 .. n `div` 2], let c = n - a - b, a ^ 2 + b ^ 2 == c ^ 2
  ]

-- Option 2 (More efficient)
findTriplets' :: Integral c => c -> [(c, c, c)]
findTriplets' n =
  [ (a, b, c) | a <- [1 .. n `div` 2], let b = (n ^ 2 - 2 * a * n) `div` (2 * (n - a)), let c = n - a - b, a < b && a ^ 2 + b ^ 2 == c ^ 2
  ]
