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
import           Control.Monad.ST (ST)

import           Data.Complex
import           Data.Monoid
import           Data.Semigroup

import           Data.Coerce
import           Data.IntMap
import           Data.Map

import           Data.Proxy
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

-- * Data.Proxy

instance Representational Proxy
instance Phantom Proxy

-- * Const

instance Representational Const where rep Coercion = Coercion
instance Representational (Const a)
instance Phantom (Const a)

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
  rep :: forall a b. Coercion a b -> Coercion (Ap f a) (Ap f b)
  rep c = case rep c :: Coercion (f a) (f b) of
    Coercion -> Coercion

instance Representational Data.Monoid.First   where rep Coercion = Coercion
instance Representational Data.Monoid.Last    where rep Coercion = Coercion

-- * containers
instance Representational (Map k) where rep Coercion = Coercion
instance Representational IntMap  where rep Coercion = Coercion
