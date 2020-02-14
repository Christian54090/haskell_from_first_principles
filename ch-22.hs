import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop
--      fmap boop doop x == (*2) ((+10) x)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop
--    ((+) <$> (*2) <*> (+10)) x
--             (*2) :: Num a => a -> a
--     (+) :: Num a => a -> a -> a
--     (+) <$> (*2) :: Num a => a -> a -> a

--    ((+) <$> (*2) <*> (+10)) 3
--    ((+) <$> (3*2) (3+10))
--    (3*2) + (3+10)
--    6 + 13
--    19

--     (+) <$> (*2) == (+) . (*2)

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap
