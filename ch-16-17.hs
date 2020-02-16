import Control.Applicative
import Data.Monoid
import Data.List (elemIndex)

-- Functors are things that can be mapped over (lists, maybe, trees, etc)
-- Functor laws:
-- Identity -
--   if we fmap the id function, it should have the same result as id
--     fmap id == id
-- Composition -
--   if we compose 2 functions, f and g, and fmap that over the same structure,
--   we should get the same result as if we fmapped them and then composed them:
--     fmap (f . g) [1..5] == fmap f . fmap g [1..5]

-- <$> == fmap

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)
-- DOES NOT FOLLOW THE IDENTITY LAW:
-- fmap id (CJust 0 "haha") => CJust 1 "haha"
-- id (CJust 0 "haha")      => CJust 0 "haha"
-- DOES NOT FOLLOW THE COMPOSITION LAW:
-- fmap (++"he") . fmap (++"ha") (CJust 0 "ho") => CJust 2 "hohahe"
-- fmap ((++"he") . (++"ha")) (CJust 0 "ho")    => CJust 1 "hohahe"

-- Applicative deals with 2 structures, smashing them together with a function
-- Structure `f` Structure
-- Applicative laws:
-- Identity:
--   pure id <*> v == id v
-- Composition:
--   'the result of composing our functions first and then applying them' and
--   'the result of applying the functions first and then composing them'
--   should be the same
--     pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--     pure (.) <*> [(+1)]<*>[(*2)] <*> [1..3] == [(+1)] <*> ([(*2)] <*> [1..3])
-- Homomorphism:
--   the effect of applying a function that is embedded in some structure to a
--   value that is embedded in some structure should be the same as applying a
--   function to a value without affecting any outside structure
--     pure f <*> pure x == pure (f x)
--     pure (+1) <*> pure 1 == pure ((+1) 1
-- Interchange:
--   u <*> pure y == pure ($ y) <*> u
listAppEx = [(+1), (*2)] <*> [2,4]
-- -> [ (+1) 2, (+1) 4, (*2) 2, (*2) 4 ]
-- => [3,5,4,8]

-- fmap f x == pure f <*> x

app :: Applicative f => f (a -> b) -> f a -> f b
app = (<*>)
-- f (a -> b) => 1st argument is a structure that holds a functor
-- f a => 2nd argument is a structure that holds an element

data Optional a = Nada | Yep a deriving (Show)

instance Functor Optional where
  fmap f Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure                = Yep
  Nada <*> _          = Nada
  _ <*> Nada          = Nada
  (Yep f) <*> (Yep a) = Yep (f a)

tupleApp = (,) <$> [1,2] <*> [3,4]
tupleApp' = liftA2 (,) [1,2] [3,4]
tupleApp'' = liftA3 (,,) [1,2] [3,4] [5,6]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y' :: Maybe Integer
y' = lookup 3 $ zip [1,2,3] [4,5,6]

z' :: Maybe Integer
z' = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z'

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant f) <*> (Constant a) = Constant $ f <> a

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) `append` (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c d) <*> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d z)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a b c d) <*> (Four' w x y z) = Four' (a <> w) (b <> x) (c <> y) (d z)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos = liftA3 (,,)