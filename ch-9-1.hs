safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

firstThree :: [a] -> Maybe [a]
firstThree []        = Nothing
firstThree (x:[])    = Nothing
firstThree (x:y:[])  = Nothing
firstThree (x:y:z:_) = Just [x,y,z]

enumFromTo' :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo' x y
  | x < y     = x : enumFromTo' (succ x) y
  | x > y     = x : enumFromTo' (pred x) y
  | otherwise = [x]

myFilter :: String -> [String]
myFilter s = filter (\x -> not (x `elem` ["the", "a", "an"])) $ words s

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _          = []
zipWith' f _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
