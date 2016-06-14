{-# Language GeneralizedNewtypeDeriving #-}

module TransformerParseer where

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

get :: (Applicative m) => StateT s m s
get = StateT $ \s -> pure (s, s)

put :: (Applicative m) => s -> StateT s m ()
put s' = StateT $ \_ -> pure ((), s')

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = get >>= put . f

type State s a = StateT s Identity a

newtype ParserT s m a = ParserT { runParserT :: StateT s m a }
                      deriving (Functor, Applicative, Monad)

instance (Alternative m, Monad m) => Alternative (ParserT s m) where
  empty = ParserT $ StateT $ \_ -> empty
  ma <|> mb = ParserT $ do
    s <- get
    let pa = runParserT ma
        pb = runParserT mb
    let _x = runStateT pa s <|> runStateT pb s
    (a, s') <- _x
    put s'
    return a

type Parser i o = ParserT i Maybe o

predParser :: (a -> Bool) -> Parser [a] a
predParser p = ParserT $ do
  l <- get
  case l of
    (x:xs) | p x -> put xs >> return x
    _            -> StateT $ \_ -> mzero

charParser :: Char -> Parser String Char
charParser c = predParser (== c)
