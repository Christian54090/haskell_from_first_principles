factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibSequence :: Integral a => a -> a -> [a]
fibSequence 0 _ = []
fibSequence t i = (fibonacci i) : fibSequence (t-1) (i+1)

sumAll :: (Eq a, Num a) => a -> a
sumAll 1 = 1
sumAll n = n + sumAll (n-1)

summation :: Integral a => a -> a -> a
summation n 1 = n
summation n t = n + summation n (t-1)
