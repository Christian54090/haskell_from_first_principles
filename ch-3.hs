main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting = (++) hello ((++) " " world)

myGreeting :: String
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

dropHead :: String -> String
dropHead x = tail x

drop' :: String -> Integer -> String
drop' str 0 = str
drop' str x = drop' (tail str) (x-1)
--drop' (x:xs) i = drop' xs (i-1)

exclaim :: String -> String
exclaim x = x ++ "!"

indexAt :: String -> Int -> String
indexAt s i = (s !! i) : ""

thirdLetter :: String -> Char
thirdLetter x = x !! 3

rvrs :: String
rvrs =
  third ++ " " ++ second ++ " " ++ first
  where
    string = "Curry is awesome"
    first = take 5 string
    second = take 2 $ drop 6 string
    third = take 7 $ drop 9 string

