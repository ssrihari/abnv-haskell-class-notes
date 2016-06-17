{-# Language GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}

module TransformerParser where

import Control.Applicative
import Control.Monad

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f = Identity . f . runIdentity

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  (Identity a) >>= f = f a

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f a = StateT $ \s -> fmap (\(b, s') -> (f b, s')) $ runStateT a s

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  f <*> a = StateT $ \s -> do
    (f', s')  <- runStateT f s
    (a', s'') <- runStateT a s'
    return (f' a', s'')

instance (Monad m) => Monad (StateT s m) where
  a >>= f = StateT $ \s -> do
    (a', s') <- runStateT a s
    runStateT (f a') s'

instance (Alternative m, Monad m) => Alternative (StateT s m) where
  empty = StateT $ \_ -> empty
  a <|> b = StateT $ \s -> runStateT a s <|> runStateT b s

class MonadState s m where
  get :: m s
  put :: s -> m ()

instance (Applicative m) => MonadState s (StateT s m) where
  get    = StateT $ \s -> pure (s, s)
  put s' = StateT $ \_ -> pure ((), s')

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = get >>= put . f

type State s a = StateT s Identity a

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  mf <*> ma = MaybeT $ (<*>) <$> runMaybeT mf <*> runMaybeT ma

instance (Monad m) => Monad (MaybeT m) where
  -- f :: a -> m b
  ma >>= f = MaybeT $ do
    a <- runMaybeT ma -- Maybe a
    case a of
      Nothing -> return Nothing
      Just a -> runMaybeT (f a)

instance (Monad m) => Alternative (MaybeT m) where
  empty = MaybeT $ pure Nothing
  a <|> b = MaybeT $ (<|>) <$> (runMaybeT a) <*> (runMaybeT b)

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- instances Func

newtype ParserT i m o = ParserT { _runParserT :: StateT i (MaybeT m) o }
                        deriving (Functor, Applicative, Monad, Alternative)

runParserT p = runMaybeT . runStateT (_runParserT p)

predParser :: Monad m => (a -> Bool) -> ParserT [a] m a
predParser p = ParserT $ do
  i <- get
  case i of
    (x:xs) | p x -> put xs >> return x
    _            -> empty

charParser :: Monad m => Char -> ParserT String m Char
charParser c = predParser (== c)
