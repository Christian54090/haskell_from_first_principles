
possiblyKaboom = \f -> f fst snd (0, undefined)

-- booleans in lamba form
true :: a -> a -> a
true = \a -> (\b -> a)
-- (\f -> f fst snd (0, undefined)) (\a -> (\b -> a))
-- (\a -> (\b -> a)) fst snd (0, undefined)
-- (\b -> fst) snd (0, undefined)
-- fst (0, undefined)
-- => 0

false :: a -> a -> a
false = \a -> (\b -> b)
-- (\f -> f fst snd (0, undefined)) (\a -> (\b -> b))
-- (\a -> (\b -> b) fst snd (0, undefined)
-- (\b -> b) snd (0, undefined)
-- snd (0, undefined)
-- => undefined

possiblyKaboom' b =
  case b of
    True  -> fst tup
    False -> snd tup
  where tup = (0, undefined)

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"

hypo' :: IO ()
hypo' = do
  let x :: Int
      x = undefined
  s <- getLine
  -- `seq` forces an evaluation of the 1st argument when the 2nd is evaluated
  case x `seq` s of
    "hi" -> print x
    _    -> putStrLn "hello"

hypo'' :: IO ()
hypo'' = do
  let x :: Integer
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"