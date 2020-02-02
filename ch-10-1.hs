foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ a []     = a
foldr' f a (x:xs) = f x (foldr' f a xs)
-- does not manipulate accumulator as it goes through the fold
-- right side exhausts and spits out accumulator, evaluates from right to left
-- breakdown foldr' (+) 0 [1,2,3]:
-- (+) 1 (foldr' (+) 0 [2,3])
-- (+) 1 ((+) 2 (foldr' (+) 0 [3]))
-- (+) 1 ((+) 2 ((+) 3 (foldr' (+) 0 [])))
-- (+) 1 ((+) 2 ((+) 3 (0)))
-- (+) 1 ((+) 2 (3))
-- (+) 1 (5)
-- => 6

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ a []     = a
foldl' f a (x:xs) = foldl' f (a `f` x) xs
-- evaluates accumulator as it goes through the fold
-- spits out evaluated accumulator at exhaustion
-- breakdown foldl' (+) 0 [1,2,3]:
-- foldl' (+) (0 + 1) [2,3]
-- foldl' (+) (1 + 2) [3]
-- foldl' (+) (3 + 3) []
-- => 6

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ a []     = [a]
scanl' f a (x:xs) = a : scanl' f (a `f` x) xs

myOr :: [Bool] -> Bool
myOr = foldr (||) False
