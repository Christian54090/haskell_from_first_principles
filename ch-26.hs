import Data.Monoid
import Control.Monad

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
  -- map over ma to access 'Maybe a', then map over 'Maybe a' to get a

instance Applicative m => Applicative (MaybeT m) where
  pure m = MaybeT (pure (pure m))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT ((<*>) <$> fab <*> mma)
  -- mma represents 'm (Maybe a)'
  -- can also be written as: 'MaybeT ((fmap (<*>) fab) <*> mma)
  --
  -- :t fmap (<*>) fab
  -- => Maybe (a -> b)
  -- lifts from outside structure, fitting the type of <*>

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)
  -- by binding ma to v you're stripping it of its outside structure, leaving
  -- you with a Maybe value

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT (pure (Right a))
  (EitherT fab) <*> (EitherT ema) = EitherT ((fmap (<*>) fab) <*> ema)

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Left a  -> return $ Left a
      Right a -> runEitherT (f a)

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT fab) <*> (ReaderT rma) = ReaderT $ (fmap (<*>) fab) <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT ma) >>= f = ReaderT $ \r -> do
    a <- ma r
    runReaderT (f a) r
