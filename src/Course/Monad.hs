{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) f = f . runExactlyOne

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  -- (=<<) f = foldRight (\x xs -> f x ++ xs) Nil
  -- (=<<) f = foldRight (++) Nil . map f
  (=<<) = flatMap

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) f (Full a) = f a
  (=<<) _ _ = Empty

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119  -- (n -> n -> n) -> (n -> n) -> (n -> n)
-- (a -> b -> a * b) -> (a' -> a+10) -> (b -> (b+10 * b))
-- (7 -> 7 + 10 * 7) -- (17 * 7) -- 119 
instance Monad ((->) t) where
  (=<<) ::
    (a -> ((->) t b))
    -> ((->) t a)
    -> ((->) t b)
  (=<<) atb ta = \t' -> atb (ta t') t' -- identical to <*> for Reader?

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
-- Implements <*> in terms of =<<

-- =<< :: a -> f b -> f a -> f b

-- <**> :: f (a -> b) -> f a -> f b
(<**>) ::
  Monad f =>
  f (a -> b)
  -> f a
  -> f b
(<**>) fab fa = (<$> fa) =<< fab

infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14

-- =<< id :: (a -> a -> b) -> a -> b 
-- a and b can be the same type
-- (a -> a -> a) -> a -> a
-- ((->a) (->a) a) -> (->a) a
-- ->a is the monad instance in this case
-- f (f a) -> f a

join ::
  Monad f =>
  f (f a)
  -> f a
join = (=<<) id

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
-- a -> f b -> f a -> f b
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip =<< = f a -> (a -> f b) -> f b

(>>=) ::
  Monad f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) = flip (=<<)

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
(<=<) ::
  Monad f =>
  (b -> f c)
  -> (a -> f b)
  -> a -> f c
(<=<) bfc afb a = bfc =<< afb a
-- bfc afb = (=<<) (bfc . afb)
-- pointfree version is (.) . (=<<)  -- No idea how to derive this from the above
-- (.) ((=<<) bcf afb a)
infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
