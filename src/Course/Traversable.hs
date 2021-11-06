{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse afb eo = ExactlyOne <$> afb (runExactlyOne eo)

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse afb (Full a) = Full <$> afb a
  traverse _ _ = pure Empty

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA = traverse id

-- traverse afb -- has the type f a -> h (f b)
-- traverse (f a -> h (f b)) :: g (f b) -> h (f (g b))
instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse afb (Compose fg) = Compose <$> traverse (traverse afb) fg
  -- can also be written as Compose <$> (traverse . traverse) afb fg

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) :: (Functor f, Functor g) => (a -> b) -> Product f g a -> Product f g b
  (<$>) ab (Product f g) = Product (ab <$> f) (ab <$> g)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse :: (Applicative h, Functor f, Functor g) => (a -> h b) -> Product f g a -> h (Product f g b)
  traverse ahb (Product f g) = 
                              -- traverse ahb f :: h (f b)
                              -- traverse ahb g :: h (g b)
                              -- lift2 gives (f b -> g b -> c) -> h (f b) -> h (g b) -> h c
                              -- here c can be Product fb gb, so the function becomes \fb gb -> Product fb gb, or just Product in pointfree
                              lift2 Product (traverse ahb f) (traverse ahb g)
            

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) :: (Functor f, Functor g) => (a -> b) -> Coproduct f g a -> Coproduct f g b
  (<$>) ab (InL fa) = InL (ab <$> fa)
  (<$>) ab (InR ga) = InR (ab <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse :: (Applicative h, Functor f, Functor g) => (a -> h b) -> Coproduct f g a -> h (Coproduct f g b)
  traverse ahb (InL fa) = InL <$> traverse ahb fa 
  traverse ahb (InR ga) = InR <$> traverse ahb ga
