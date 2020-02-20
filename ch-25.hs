import Data.Monoid

-- a monad transformer is a type constructor that can take a monad as an
-- argument

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)
-- f & g must be type constructors themselves, while a is a concrete type
-- :t Compose [Just (1::Int), Nothing]
-- => Compose [Just (1::Int), Nothing] :: Compose [] Maybe Int
--                                                f  ( g    a )

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
  -- mapping into f to access (g a), then mapping into g to access (a)

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  (Compose f) <*> (Compose a) = Compose $ (fmap (\f' -> (f' <*>)) f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga
  --                        foldMap f (f (g a)) | foldMap f ([Just 1])
  --                        foldMap f (g a)     | foldMap f (Just 1)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
  -- traverse into f to access (g a), then traverse into g to access (a)

-- BIFUNCTOR --

class BiFunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b deriving Show

instance BiFunctor Deux where
  first f (Deux a b)   = Deux (f a) b
  second f (Deux a b)  = Deux a (f b)
  bimap f g (Deux a b) = Deux (f a) (g b)

data Quadriceps a b c d = Quads a b c d deriving (Eq, Show)

instance BiFunctor (Quadriceps a b) where
  bimap f g (Quads a b c d) = Quads a b (f c) (g d)

-- IDENTITY T --

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)
  -- fab represents f (a -> b)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
--[     1      ] [2][3]  [   8   ]  [4] [5] [    7    ]   [6]
--
-- 1. first we unpack the 'm a' value of IdentityT
-- 2. the type of the bind:
--      (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
-- 3. the function we're binding over is IdentityT m a. it has the the type:
--      (a -> IdentityT m b)
-- 4. here ma is the same one we unpacked. now removed from IdentityT context
-- 5. this is a different bind. we're now using the monad from the instance
--    declaration with the constraint 'Monad m =>'. this has the type:
--      (>>=) :: m a -> (a -> m b) -> m b
-- 6. this is the same f which was an argument at (3). still has the same type
-- 7. we need runIdentityT because f returns 'IdentityT m b' but the '>>=' for
--    the monad instance has the type 'm a -> (a -> m b) -> m b'. the type of f
--    is incompatible because 'IdentityT m' /= 'm'. we use runIdentityT to
--    unpack the value. runIdentityT has the type 'IdentityT m b -> m b'
-- 8. satisfies the type of the outer bind, which expects the type
--      IdentityT m b