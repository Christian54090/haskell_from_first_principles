foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f a []     = a
foldr' f a (x:xs) = f x (foldr' f a xs)
-- f = function, a = accumulator, xs = input list
-- foldr' (+) 0 [1,2,3] ==
-- (+) 1 ((+) 2 ((+) 3 0))
--  6 <--- 5 <--- 3

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs
-- foldl' (+) 0 [1,2,3] ==
-- (((+) 0 1) (+) 2) (+) 3)
--    1 -----> 3 ---> 6
-- ((0 + 1) + 2) + 3

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))
