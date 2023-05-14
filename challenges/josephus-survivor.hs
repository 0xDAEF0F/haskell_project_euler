josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = js n k + 1

js :: Int -> Int -> Int
js 1 _ = 0
js n k = (js (n - 1) k + k) `mod` n
