safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool _ False = []
eftBool True _  = [True]
eftBool a b     = [a] ++ eftBool (succ a) b

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd _ LT = []
eftOrd GT _ = [GT]
eftOrd a b  = [a] ++ eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b     = error "End is less than beginning"
  | a == b    = [a]
  | otherwise = [a] ++ eftInt (succ a) b

eftInt' :: Int -> Int -> [Int]
eftInt' a b
  | a > b     = [a] ++ eftInt' (pred a) b
  | a < b     = [a] ++ eftInt' (succ a) b
  | otherwise = [a]

eftChar :: Char -> Char -> [Char]
eftChar a b
  | a > b     = [a] ++ eftChar (pred a) b
  | a < b     = [a] ++ eftChar (succ a) b
  | otherwise = [a]
