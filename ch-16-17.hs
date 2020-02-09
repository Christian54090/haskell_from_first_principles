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
