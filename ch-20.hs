-- Foldable gives us a way to process values embedded in a structure as if they
-- existed in a sequential order

import Data.Monoid
import Data.Foldable

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = Nada | Yep a --deriving (Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

sum'' :: (Foldable t, Num a) => t a -> Sum a
sum'' = foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a) => t a -> Product a
product'' = foldMap Product

length' :: (Foldable t) => t a -> Int
length' = foldr (\x y -> (const 1 x) + y) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldl f z (Two a b) = f z b
  foldMap f (Two a b) = f b

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldr f z (Four a b1 b2 b3) = f b1 (f b2 (f b3 z))

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool)
        -> t a
        -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
