{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  -- <$> . <$> ::  a -> b -> f a -> f b -> f (g a) -> f (g b)
  (<$>) f' (Compose fg) = Compose $ ((<$>) . (<$>)) f' fg

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
 -- pure :: a -> f a   pure . pure :: a -> f a -> f (g a)
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
 -- <*> :: f (a -> b) -> f a -> f b
 -- <*> :: f (g (a -> b)) -> f (g a) -> f (g b) -> f (g a -> g b) ... f (g b)
 -- Fmapping <*> into the inner functor has the type (f (g (a -> b) -> f (g a -> g b)))
 -- This allows f (g a -> g b) to be isomorphic to f (a -> b) in a regular Applicative
  (<*>) (Compose fgab) (Compose fga) = Compose $ (<$>) (<*>) fgab <*> fga

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
-- a -> f b  -> f a -> f b
-- a -> f (g b) -> f (g a) -> f (g b)
-- Using <$> gets us f (g (f (g b))), but we can't use join as the monads will always be interleaved like this
  (=<<) = undefined
