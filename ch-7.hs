
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

f :: (a,b,c)
  -> (d,e,f)
  -> ((a,d),(c,f))
f (a,b,c) (d,e,f) = ((a,d),(c,f))

ifEvenAdd2 n = if (even n) then (n+2) else n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
