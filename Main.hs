module Main where

-- Option 1
main = getLine >>= \l -> putStrLn $ reverse l

-- Option 2
-- main = do
--   line <- getLine
--   putStrLn $ reverse line