import Control.Monad
import Data.Monoid
import Test.QuickCheck

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

type S = String
type B = Bool

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv n adj =
  e   <> "! he said " <>
  adv <> " as he jumped into his car " <>
  n   <> " ad drove off with his " <>
  adj <> " wife."

--madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
--madlibbinBetter' e adv n adj =

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = (mempty <> a) == a

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = (a <> mempty) == a

monoidLeftId' :: (Eq m, Monoid m) => m -> Bool
monoidLeftId' a = mappend a mempty == a

-- mappend == (<>)

-- (Sum 1) <> (Sum 2) <> (Sum 3) == Sum {getSum = 6}

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada
  mappend (Only a) Nada     = Only a
  mappend Nada (Only a)     = Only a
  mappend (Only a) (Only b) = Only (a <> b) -- Only $ a <> b same thing

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    b <- arbitrary
    frequency [(1, return Nada), (3, return (Only b))]

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) x = x
  mappend x (First' Nada) = x
  mappend x _             = x

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    b <- arbitrary
    return (First' b)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' S -> First' S -> First' S -> B

type FstId = First' S -> B

newtype Identity a = Identity a deriving (Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity $ x <> y

data Two a b = Two a b deriving Show

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b) (Two x y) = Two (a <> x) (b <> y)

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  mappend (Combine f) (Combine g) = Combine (\x -> f x <> g x)

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)
  mappend (Mem f) (Mem g) =
    Mem $ \x ->
      (fst (f x) <> fst (g x), snd (f (snd (g x))))

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero  = runMem mempty 0
      rmleft  = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0