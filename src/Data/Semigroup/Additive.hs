{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:
--   Data.Semigroup.Additive
-- Description:
--   This module defines a class for commutative semigroups, until it is moved
--   to another library.
module Data.Semigroup.Additive
  ( Additive
  ) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity
-- For base-orphans, TODO don't cheat.
import Data.Map.Monoidal ()
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

-- | An 'Additive' 'Semigroup' is one where (<>) is commutative
class Semigroup q => Additive q where

-- | Trivial group.
instance Additive ()

-- | Product group.  A Pair of groups gives rise to a group
instance (Additive a, Additive b) => Additive (a, b)

-- See https://gitlab.haskell.org/ghc/ghc/issues/11135#note_111802 for the reason Compose is not also provided.
-- Base does not define Monoid (Compose f g a) so this is the best we can
-- really do for functor composition.
instance Additive (f (g a)) => Additive ((f :.: g) a)

-- | Product of groups, Functor style.
instance (Additive (f a), Additive (g a)) => Additive ((f :*: g) a)

-- | Trivial group, Functor style
instance Additive (Proxy x)

-- | Const lifts groups into a functor.
instance Additive a => Additive (Const a x)
-- | Ideitnty lifts groups pointwise (at only one point)
instance Additive a => Additive (Identity a)

-- | Functions lift groups pointwise.
instance Additive b => Additive (a -> b)
