h :: (Num a, Num b) => a -> b -> b
h = undefined

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

myMult :: (Fractional a) => a -> a
myMult x = (x/3) * 5

myTake :: Int -> [Char]
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > (length [1..10])

myAlph :: Char -> Bool
myAlph x = x < 'z'
