data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
       weekday == weekday'
    && dayOfMonth == dayOfMonth'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn integer)
       (TisAn integer') =
       integer == integer'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two integer1 integer2)
       (Two integer1' integer2') =
       integer1 == integer1'
    && integer2 == integer2'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int)
       (TisAnInt int') =
       int == int'
  (==) (TisAString str)
       (TisAString str') =
       str == str'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x
