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
  
  -- * Utility functions to define Representable easily
  , Proxy2(..)
  , wrapped
  , slash
  , coercionOf
  , eta
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

import Control.Monad.Reader      (ReaderT(..))
import Control.Monad.Except      (ExceptT(..), runExceptT)
import Control.Monad.Cont        (ContT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Writer.Lazy   as L (WriterT(..), runWriterT)
import qualified Control.Monad.Writer.Strict as S (WriterT(..), runWriterT)
import qualified Control.Monad.State.Lazy    as L (StateT(..), runStateT)
import qualified Control.Monad.State.Strict  as S (StateT(..), runStateT)
import qualified Control.Monad.RWS.Lazy      as L (RWST(..), runRWST)
import qualified Control.Monad.RWS.Strict    as S (RWST(..), runRWST)

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
           (Coercible f g) => proxy f g -> Coercion (f a) (f b) -> Coercion (g a) (g b)
wrapped _ c = (Coercion :: Coercion (g a) (f a)) >>> c >>> (Coercion :: Coercion (f b) (g b))

data Proxy2 a b = Proxy2

slash ::  proxy (f a) (g a) -> Proxy2 f g
slash _ = Proxy2

coercionOf :: Coercible a b => proxy a b -> Coercion a b
coercionOf _ = Coercion

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
  rep c = coercionOf getCompose >>> rep (rep c) >>> coercionOf Compose

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
  rep c = wrapped (slash WrapMonad) (rep c)

instance Representational WrappedArrow where rep Coercion = Coercion
instance (Representational p) => Representational (WrappedArrow p) where
  rep c = wrapped (slash (slash WrapArrow)) (rep c)
instance (Representational (p a)) => Representational (WrappedArrow p a) where
  rep c = wrapped (slash WrapArrow) (rep c)

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
  rep c = wrapped (slash Ap) (rep c)

instance Representational Data.Monoid.First   where rep Coercion = Coercion
instance Representational Data.Monoid.Last    where rep Coercion = Coercion

-- * containers
instance Representational (Map k) where rep Coercion = Coercion
instance Representational IntMap  where rep Coercion = Coercion

-- * mtl, transformers
instance Representational ReaderT     where rep Coercion = Coercion
instance Representational (ReaderT r) where rep Coercion = Coercion
instance (Representational m)
      => Representational (ReaderT r m) where
  rep c = coercionOf runReaderT >>> rep (rep c) >>> coercionOf ReaderT

instance (Representational m)
      => Representational (ExceptT e m) where
  rep c = coercionOf runExceptT >>> rep (rep c) >>> coercionOf ExceptT

instance Representational (ContT r)   where rep Coercion = Coercion
instance Representational (ContT r m) where rep Coercion = Coercion

instance Representational MaybeT where rep Coercion = Coercion
instance (Representational m)
      => Representational (MaybeT m) where
  rep c = coercionOf runMaybeT >>> rep (rep c) >>> coercionOf MaybeT

firstR :: Coercion a b -> Coercion (a,c) (b,c)
firstR = eta . rep

instance (Representational m)
      => Representational (L.WriterT w m) where
  rep c = coercionOf L.runWriterT >>> rep (firstR c) >>> coercionOf L.WriterT

instance (Representational m)
      => Representational (S.WriterT w m) where
  rep c = coercionOf S.runWriterT >>> rep (firstR c) >>> coercionOf S.WriterT

instance (Representational m)
      => Representational (L.StateT s m) where
  rep c = coercionOf L.runStateT >>> rep (rep (firstR c)) >>> coercionOf L.StateT

instance (Representational m)
      => Representational (S.StateT s m) where
  rep c = coercionOf S.runStateT >>> rep (rep (firstR c)) >>> coercionOf S.StateT

instance Representational L.RWST where rep Coercion = Coercion
instance (Representational m)
      => Representational (L.RWST e w s m) where
  --                                     c :: a ~R b
  --                               rep $ c :: (,,) a ~R (,,) b
  --                   eta . eta . rep $ c :: (a,w,s) ~R (b,w,s)
  --             rep . eta . eta . rep $ c :: m (a,w,s) ~R m (b,w,s)
  -- rep . rep . rep . eta . eta . rep $ c :: e -> s -> m (a,w,s) ~R e -> s -> m (b,w,s)
  rep c = coercionOf L.runRWST >>> (rep . rep . rep . eta . eta . rep) c >>> coercionOf L.RWST

instance Representational S.RWST where rep Coercion = Coercion
instance (Representational m)
      => Representational (S.RWST e w s m) where
  rep c = coercionOf S.runRWST >>> (rep . rep . rep . eta . eta . rep) c >>> coercionOf S.RWST
