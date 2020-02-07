import Control.Monad
import Data.Monoid
import Test.QuickCheck

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

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
