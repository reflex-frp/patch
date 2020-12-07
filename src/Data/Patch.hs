{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:
--   Data.Patch
-- Description:
--   This module defines the 'Patch' class.
module Data.Patch
  ( module Data.Patch
  , module X
  ) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

import Data.Patch.Class as X
import Data.Patch.DMap as X hiding (getDeletions)
import Data.Patch.DMapWithMove as X
  ( PatchDMapWithMove, const2PatchDMapWithMoveWith, mapPatchDMapWithMove
  , patchDMapWithMoveToPatchMapWithMoveWith
  , traversePatchDMapWithMoveWithKey, unPatchDMapWithMove
  , unsafePatchDMapWithMove, weakenPatchDMapWithMoveWith
  )
import Data.Patch.IntMap as X hiding (getDeletions)
import Data.Patch.Map as X
import Data.Patch.MapWithMove as X
  ( PatchMapWithMove, patchMapWithMoveNewElements
  , patchMapWithMoveNewElementsMap, unPatchMapWithMove
  , unsafePatchMapWithMove
  )

-- | An 'Additive' 'Semigroup' is one where (<>) is commutative
class Semigroup q => Additive q where

-- | The elements of an 'Additive' 'Semigroup' can be considered as patches of their own type.
newtype AdditivePatch p = AdditivePatch { unAdditivePatch :: p }

instance Additive p => Patch (AdditivePatch p) where
  type PatchTarget (AdditivePatch p) = p
  apply (AdditivePatch p) q = Just $ p <> q

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
