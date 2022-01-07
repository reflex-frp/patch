{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Description:
  This module defines the 'Group' class, and reexports the other modules.
-}
module Data.Patch
  ( module Data.Patch
  , module X
  ) where

import Data.Semigroup.Commutative
import Data.Map.Monoidal (MonoidalMap)

import Data.Group as X (Group (..))
import Data.Semigroup.Additive as X
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

-- | A 'Group' is a 'Monoid' where every element has an inverse.
class (Semigroup q, Monoid q) => Group q where
  negateG :: q -> q
  (~~) :: q -> q -> q
  r ~~ s = r <> negateG s

-- | The elements of an 'Commutative' 'Semigroup' can be considered as patches of their own type.
newtype AdditivePatch p = AdditivePatch { unAdditivePatch :: p }

instance Commutative p => Patch (AdditivePatch p) where
  type PatchTarget (AdditivePatch p) = p
  apply (AdditivePatch p) q = Just $ p <> q

negateG :: Group g => g -> g
negateG = invert

-- TODO move orphan
instance (Ord k, Group q) => Group (MonoidalMap k q) where
  invert = fmap invert
