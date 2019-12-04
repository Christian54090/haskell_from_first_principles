sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

piTimesXY x y = 3.14 * (x * y)

waxOn =
  x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

waxOff x = (/10) $ triple x
