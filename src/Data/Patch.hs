{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description:
  This module defines the 'Group' class, and reexports the other modules.
-}
module Data.Patch
  ( module Data.Patch
  , module X
  ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

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

-- | The elements of an 'Additive' 'Semigroup' can be considered as patches of their own type.
newtype AdditivePatch p = AdditivePatch { unAdditivePatch :: p }

instance Additive p => Patch (AdditivePatch p) where
  type PatchTarget (AdditivePatch p) = p
  apply (AdditivePatch p) q = Just $ p <> q
