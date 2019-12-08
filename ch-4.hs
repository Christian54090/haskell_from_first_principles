data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
  if x == reverse x
    then True
  else
    False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' x = reverse x == x

myAbs :: Integer -> Integer
myAbs x =
  if x < 0
    then x - (2*x)
  else
    x

myFun :: (a,b) -> (c,d) -> ((b,d),(a,c))
myFun ab cd = ((snd ab, snd cd), (fst ab, fst cd))
