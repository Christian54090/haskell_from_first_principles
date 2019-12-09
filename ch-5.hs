-- not :: Bool -> Bool
-- length :: Foldable t => t a -> Int
-- concat :: Foldable t => t [a] -> [a]
-- head :: [a] -> a
-- (<) :: Ord a => a -> a -> Bool

h :: (Num a, Num b) => a -> b -> b
h a b = undefined

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = undefined

kessel :: (Ord a, Num b) => a -> b -> a
kessel a b = undefined

myConcat x = x ++ " yo"
myMult x = (x / 3) * 5
myTake x = take x "hey you"
myCom x = x > (length [1..10])
myAlph x = x < 'z'

functionN :: [Char] -> Char
functionN (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i = id

r :: [a] -> [a]
r = tail

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
