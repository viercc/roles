{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Roles
  ( Representational(rep)
  , Phantom(phantom)
  ) where

import           Control.Applicative
import           Control.Category      ((>>>))
import           Control.Monad.ST      (ST)

import           Data.Complex
import           Data.Monoid
import           Data.Semigroup

import           Data.Proxy

import           Data.Functor.Identity
import           Data.Functor.Compose
import qualified Data.Functor.Product  as F
import qualified Data.Functor.Sum      as F

import           Data.IntMap
import           Data.Map

import           Data.Coerce
import           Data.Type.Coercion
import           Unsafe.Coerce

class Representational (t :: k1 -> k2) where
  -- | An argument is representational if you can lift a coercion of the argument into one of the whole
  rep :: Coercion a b -> Coercion (t a) (t b)
  default rep :: Phantom t => Coercion a b -> Coercion (t a) (t b)
  rep _ = phantom

class Representational t => Phantom (t :: k1 -> k2) where
  -- | An argument is phantom if you can 'coerce' the whole ignoring the argument
  phantom :: Coercion (t a) (t b)
  default phantom :: Coercible (t a) (t b) => Coercion (t a) (t b)
  phantom = Coercion

wrapped :: forall x y proxy (f :: x -> y) (g :: x -> y) (a :: x) (b :: x).
           (Coercible f g) => proxy f g ->  Coercion (f a) (f b) -> Coercion (g a) (g b)
wrapped _ c = (Coercion :: Coercion (g a) (f a)) >>> c >>> (Coercion :: Coercion (f b) (g b))

data P a b = P
data X a = X

(//) ::  p (f a) (g a) -> q a -> P f g
_ // _ = P

infixl 8 //

eta :: forall f g a. Coercion f g -> Coercion (f a) (g a)
eta Coercion = Coercion

-- * Data.Proxy

instance Representational Proxy
instance Phantom Proxy

-- * Data.Functor.{Const, Identity, Sum, Product, Compose}

instance Representational Const where rep Coercion = Coercion
instance Representational (Const a)
instance Phantom (Const a)

instance Representational Identity where rep Coercion = Coercion

instance Representational F.Sum     where rep Coercion = Coercion
instance Representational (F.Sum f) where rep Coercion = Coercion

instance Representational F.Product where rep Coercion = Coercion
instance Representational (F.Product f) where rep Coercion = Coercion

instance Representational Compose where rep Coercion = Coercion
instance (Representational f,
          Representational g)
      => Representational (Compose f g) where
  rep :: forall a b. Coercion a b -> Coercion (Compose f g a) (Compose f g b)
  rep c = (Coercion :: Coercion (Compose f g a) (f (g a))) >>>
          rep (rep c) >>>
          (Coercion :: Coercion (f (g b)) (Compose f g b))

-- * Data.Type.Coercion

instance Representational Coercion     where rep = unsafeCoerce
instance Representational (Coercion a) where rep Coercion = Coercion

-- * Prelude

instance Representational (->)       where rep Coercion = Coercion
instance Representational ((->) a)   where rep Coercion = Coercion

instance Representational Either     where rep Coercion = Coercion
instance Representational (Either a) where rep Coercion = Coercion

instance Representational (,)     where rep Coercion = Coercion
instance Representational ((,) a) where rep Coercion = Coercion

instance Representational (,,)       where rep Coercion = Coercion
instance Representational ((,,) a)   where rep Coercion = Coercion
instance Representational ((,,) a b) where rep Coercion = Coercion

instance Representational (,,,)         where rep Coercion = Coercion
instance Representational ((,,,) a)     where rep Coercion = Coercion
instance Representational ((,,,) a b)   where rep Coercion = Coercion
instance Representational ((,,,) a b c) where rep Coercion = Coercion

instance Representational (,,,,)           where rep Coercion = Coercion
instance Representational ((,,,,) a)       where rep Coercion = Coercion
instance Representational ((,,,,) a b)     where rep Coercion = Coercion
instance Representational ((,,,,) a b c)   where rep Coercion = Coercion
instance Representational ((,,,,) a b c d) where rep Coercion = Coercion

instance Representational (,,,,,)             where rep Coercion = Coercion
instance Representational ((,,,,,) a)         where rep Coercion = Coercion
instance Representational ((,,,,,) a b)       where rep Coercion = Coercion
instance Representational ((,,,,,) a b c)     where rep Coercion = Coercion
instance Representational ((,,,,,) a b c d)   where rep Coercion = Coercion
instance Representational ((,,,,,) a b c d e) where rep Coercion = Coercion

instance Representational []      where rep Coercion = Coercion
instance Representational Maybe   where rep Coercion = Coercion
instance Representational IO      where rep Coercion = Coercion
instance Representational (ST s)  where rep Coercion = Coercion

-- * Control.Applicative

instance Representational ZipList where rep Coercion = Coercion

instance Representational WrappedMonad where rep Coercion = Coercion
instance Representational m => Representational (WrappedMonad m) where
  rep c = wrapped (WrapMonad // X) (rep c)

instance Representational WrappedArrow where rep Coercion = Coercion
instance (Representational p) => Representational (WrappedArrow p) where
  rep c = wrapped (WrapArrow // X // X) (rep c)
instance (Representational (p a)) => Representational (WrappedArrow p a) where
  rep c = wrapped (WrapArrow // X) (rep c)

-- * Data.Complex

instance Representational Complex where rep Coercion = Coercion

-- * Data.Semigroup

instance Representational Arg     where rep Coercion = Coercion
instance Representational (Arg a) where rep Coercion = Coercion
instance Representational Product where rep Coercion = Coercion
instance Representational Sum     where rep Coercion = Coercion
instance Representational Dual    where rep Coercion = Coercion
instance Representational Endo    where rep Coercion = Coercion
instance Representational Option  where rep Coercion = Coercion

instance Representational Data.Semigroup.First where rep Coercion = Coercion
instance Representational Data.Semigroup.Last  where rep Coercion = Coercion
instance Representational WrappedMonoid        where rep Coercion = Coercion

-- * Data.Monoid

instance Representational Ap                  where rep Coercion = Coercion
instance Representational f => Representational (Ap f) where
  rep c = wrapped (Ap // X) (rep c)

instance Representational Data.Monoid.First   where rep Coercion = Coercion
instance Representational Data.Monoid.Last    where rep Coercion = Coercion

-- * containers
instance Representational (Map k) where rep Coercion = Coercion
instance Representational IntMap  where rep Coercion = Coercion
