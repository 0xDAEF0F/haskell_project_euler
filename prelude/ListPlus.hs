module ListPlus where

-- Very primitive. Look at the nesting.
listPlus :: Num a => [a] -> [a] -> [a]
listPlus la lb = case la of
  [] -> []
  a : as -> case lb of
    [] -> []
    bs -> map (+ a) bs ++ listPlus as bs

-- Using applicative.
listPlus' :: (Applicative f, Num b) => f b -> f b -> f b
listPlus' la lb = (+) <$> la <*> lb

-- Using list comprehensions.
listPlus'' :: Num a => [a] -> [a] -> [a]
listPlus'' la lb = [x + y | x <- la, y <- lb]

-- Using our defined `andThen` operator
listPlus''' :: Num a => [a] -> [a] -> [a]
listPlus''' la lb = la `andThen` (\a -> lb `andThen` (\b -> [a + b]))

-- This one works for all monads because of the return (inserts to monadic context).
listPlus'''' :: (Monad m, Num b) => m b -> m b -> m b
listPlus'''' la lb = la >>= (\a -> lb >>= (\b -> return (a + b)))

-- >>= for lists.
andThen :: [t] -> (t -> [a]) -> [a]
andThen [] _ = []
andThen (x : xs) f = f x ++ andThen xs f