muls :: Int -> Int
muls n = sum $ filter mul3Or5 [1..n]
    where mul3Or5 n = n `mod` 3 == 0 || n `mod` 5 == 0
