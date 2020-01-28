exclaim :: String -> String
exclaim s = s ++ "!"

drop9 :: [a] -> [a]
drop9 s = drop 9 s

index' :: [a] -> a
index' s = s !! 4

rvrs :: String
rvrs =
  third ++ " " ++ second ++ " " ++ first
  where
    str = "Curry is awesome"
    first = take 5 str
    second = take 2 (drop 6 str)
    third = drop 9 str

