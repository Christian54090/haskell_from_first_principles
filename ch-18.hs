import Control.Monad (join, liftM2, liftM3, (>=>))
import Control.Applicative

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

-- what Monad is not:
-- impure. monadic functions are pure functions
-- an embedded language for imperative programming.
-- a value.
-- about strictness. the monadic operations of bind and return are non-strict

-- the monad typeclass is generalized structure manipulation with some laws
-- to make it sensible.

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("why hello there " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "hello friend." >>
  putStrLn "name pls: " >>
  getLine >>= (\name -> putStrLn ("why hello there " ++ name))

-- Monad laws:
-- IDENTITY:
--   right identity
--     m >>= return == m
--   left identity
--     return x >>= f == f x
--   ****return should be neutral and not perform any computation
-- ASSOCIATIVITY:
--   regrouping the functions should not have any impact on the final result
--     (m >>= f) >>= g == m >>= (\x -> f x >>= g)

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))
-- using join and fmap(<$>) together means we can use >>=

mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f

sayHi :: String -> IO String
sayHi g = do
  putStrLn g
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello, how old are you? "

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure                      = Second
  (First a) <*> _           = First a
  _ <*> (First a)           = First a
  (Second f) <*> (Second a) = Second (f a)

instance Monad (Sum a) where
  return = pure
  (First a)  >>= _ = First a
  (Second b) >>= f = f b

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) `append` (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = (f x) `append` (xs >>= f)

append :: List a -> List a -> List a
append xs Nil = xs
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

j :: Monad m => m (m a) -> m a
j x = x >>= fmap id

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = f x >>= (\b -> fmap (b:) (meh xs f))