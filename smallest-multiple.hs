import Data.List (find)

smallestMultiple upTo = find canDivideByRange [upTo..]
    where canDivideByRange x = all (\a -> x `mod` a == 0) [1..upTo] 
