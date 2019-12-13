factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

---------------------

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

sumAll :: Integer -> Integer
sumAll 0 = 0
sumAll n = n + sumAll (n-1)

sumAll' :: (Eq a, Num a) => a -> a
sumAll' n
  | n == 0    = n
  | otherwise = n + sumAll' (n-1)

summation :: (Integral a) => a -> a -> a
summation _ 0 = 0
summation x y = x + summation x (y-1)
