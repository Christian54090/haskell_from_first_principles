data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = reverse s == s

myAbs :: Integer -> Integer
myAbs x =
  if x < 0
    then x * (-1)
  else
    x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
