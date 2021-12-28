{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : A class for commutative semigroups
-}
module Data.Semigroup.Additive
  ( Additive
  ) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity
import Data.Proxy
#if !MIN_VERSION_base(4,12,0)
-- for :*: and :.: semigroup instances
import Data.Orphans ()
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

-- | An 'Additive' 'Semigroup' is one where (<>) is commutative
class Semigroup q => Additive q where

-- | Trivial additive semigroup.
instance Additive ()

-- | Product additive semigroup.
-- A Pair of additive semigroups gives rise to a additive semigroup
instance (Additive a, Additive b) => Additive (a, b)

-- See https://gitlab.haskell.org/ghc/ghc/issues/11135#note_111802 for the reason Compose is not also provided.
-- Base does not define Monoid (Compose f g a) so this is the best we can
-- really do for functor composition.
instance Additive (f (g a)) => Additive ((f :.: g) a)

-- | Product of additive semigroups, Functor style.
instance (Additive (f a), Additive (g a)) => Additive ((f :*: g) a)

-- | Trivial additive semigroup, Functor style
instance Additive (Proxy x)

-- | Const lifts additive semigroups into a functor.
instance Additive a => Additive (Const a x)

-- | Identity lifts additive semigroups pointwise (at only one point)
instance Additive a => Additive (Identity a)

-- | Functions lift additive semigroups pointwise.
instance Additive b => Additive (a -> b)
