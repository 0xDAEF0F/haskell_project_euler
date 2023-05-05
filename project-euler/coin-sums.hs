import Data.Map (Map, (!))
import Data.Map qualified as M

f :: [Int] -> Int -> Int
f [] _ = 0
f c@(x : xs) t
  | t == 0 = 1
  | t >= x = f c (t - x) + f xs t
  | otherwise = f xs t

coinSums coinsList target = f coinsList target M.empty
  where
    f c@(x : xs) t m =
      case M.lookup (t, c) m of
        Just v -> (v, m)
        Nothing ->
          let (ways1, m1) = f c (t - x) m
              (ways2, m2) = f xs t m1
              ways = ways1 + ways2
           in (ways, M.insert (t, c) ways m2)