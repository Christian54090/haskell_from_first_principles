import Data.Monoid

-- Traversable allows you to transform elements inside the structure like a
-- functor, producing applicative effects along the way, and lift those
-- potentially multiple instances of applicative structure outside of the
-- traversable structure

-- traverse a data structure, mapping a function inside a structure while
-- accumulating the applicative contexts along the way

fmapJust = fmap Just [1,2,3]
-- => [Just 1, Just 2, Just 3]
seqAJust = sequenceA $ fmap Just [1,2,3]
-- => Just [1,2,3]
seqANothing = sequenceA [Just 1, Just 2, Nothing]
-- => Nothing

traverse' :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA . fmap f

-- any time you need to flip two type constructors around, or map something
-- and then flip them around, that's probably Traversable

-- Traversable laws:
-- NATURALITY
--   function composition behaves in unsurprising ways with respect to a
--   traversed function
--     t . traverse f = traverse (t . f)
-- IDENTITY
--   traversing the data constructor of the Identity type over a value will
--   produce the same result as just putting the value in identity
-- COMPOSITION
--   we can collapse sequential traversals into a single traversal, by taking
--   advantage of the Compose datatype, which combines structure
--     traverse (Compose . fmap g . f) =
--       Compose . fmap (traverse g) . traverse f

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
--
--instance Applicative Identity where
--  pure             = Identity
--  Identity f <*> r = fmap f r

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

data Optional a = Nada | Yep a

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a
  foldr f z Nada    = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)
  foldr _ z Nil         = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node xs y zs) = Node (fmap f xs) (f y) (fmap f zs)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node xs y zs) = (foldMap f xs) <> (f y) <> (foldMap f zs)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node xs y zs) =
    Node <$> (traverse f xs) <*> (f y) <*> (traverse f zs)

tree :: Num a => Tree a
tree =
  Node (Node (Leaf 1) 2 Empty)
       3
       (Node (Leaf 4) 5 (Leaf 6))