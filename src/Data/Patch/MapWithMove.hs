{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Patch'es on 'Map' that can insert, delete, and move values from one key to
-- another
module Data.Patch.MapWithMove
  ( module Data.Patch.MapWithMove
  , PatchMapWithMove (PatchMapWithMove)
  , NodeInfo
  , pattern PM.NodeInfo
  , PM._nodeInfo_to
  , PM._nodeInfo_from
  ) where

import Data.Patch.Class
import Data.Patch.MapWithPatchingMove
  ( PatchMapWithPatchingMove (..)
  )
import qualified Data.Patch.MapWithPatchingMove as PM

import Control.Lens hiding (from, to)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup (Semigroup (..))
#endif
import Data.Traversable (foldMapDefault)

-- | Patch a Map with additions, deletions, and moves.  Invariant: If key @k1@
-- is coming from @From_Move k2@, then key @k2@ should be going to @Just k1@,
-- and vice versa.  There should never be any unpaired From/To keys.
newtype PatchMapWithMove k v = PatchMapWithMove'
  { -- | Extract the underlying 'PatchMapWithPatchingMove k (Proxy v)'
    unPatchMapWithMove' :: PatchMapWithPatchingMove k (Proxy v)
  }
  deriving ( Show, Read, Eq, Ord
           , Semigroup, Monoid
           )

{-# COMPLETE PatchMapWithMove #-}
pattern PatchMapWithMove :: Map k (NodeInfo k v) -> PatchMapWithMove k v
pattern PatchMapWithMove m = PatchMapWithMove' (PatchMapWithPatchingMove m)

-- | Extract the internal representation of the 'PatchMapWithMove'
unPatchMapWithMove :: PatchMapWithMove k v -> Map k (PM.NodeInfo k (Proxy v))
unPatchMapWithMove = unPatchMapWithPatchingMove . unPatchMapWithMove'

instance Functor (PatchMapWithMove k) where
  fmap f = runIdentity . traverse (Identity . f)

instance Foldable (PatchMapWithMove k) where
  foldMap = foldMapDefault

instance Traversable (PatchMapWithMove k) where
  traverse =
    _Wrapping PatchMapWithMove' .
    _Wrapping PatchMapWithPatchingMove .
    traverse .
    traverseNodeInfo

instance FunctorWithIndex k (PatchMapWithMove k)
instance FoldableWithIndex k (PatchMapWithMove k)
instance TraversableWithIndex k (PatchMapWithMove k) where
  itraverse = itraversed . Indexed
  itraversed =
    _Wrapping PatchMapWithMove' .>
    _Wrapping PatchMapWithPatchingMove .>
    itraversed <.
    traverseNodeInfo

type NodeInfo k v = PM.NodeInfo k (Proxy v)

type From k v = PM.From k (Proxy v)

{-# COMPLETE From_Insert, From_Delete, From_Move #-}

pattern From_Insert :: v -> From k v
pattern From_Insert v = PM.From_Insert v

pattern From_Delete :: From k v
pattern From_Delete = PM.From_Delete

pattern From_Move :: k -> From k v
pattern From_Move k = PM.From_Move k Proxy

type To k = PM.To k

traverseNodeInfo
  :: Traversal (NodeInfo k a) (NodeInfo k b) a b
traverseNodeInfo = PM.bitraverseNodeInfo pure (\(~Proxy) -> pure Proxy)

-- | Create a 'PatchMapWithMove', validating it
patchMapWithMove :: Ord k => Map k (NodeInfo k v) -> Maybe (PatchMapWithMove k v)
patchMapWithMove = fmap PatchMapWithMove' . PM.patchMapWithPatchingMove

-- | Create a 'PatchMapWithMove' that inserts everything in the given 'Map'
patchMapWithMoveInsertAll :: Map k v -> PatchMapWithMove k v
patchMapWithMoveInsertAll = PatchMapWithMove' . PM.patchMapWithPatchingMoveInsertAll

-- | Make a @'PatchMapWithMove' k v@ which has the effect of inserting or updating a value @v@ to the given key @k@, like 'Map.insert'.
insertMapKey :: k -> v -> PatchMapWithMove k v
insertMapKey k v = PatchMapWithMove' $ PM.insertMapKey k v

-- |Make a @'PatchMapWithMove' k v@ which has the effect of moving the value from the first key @k@ to the second key @k@, equivalent to:
--
-- @
--     'Map.delete' src (maybe map ('Map.insert' dst) (Map.lookup src map))
-- @
moveMapKey :: Ord k => k -> k -> PatchMapWithMove k v
moveMapKey src dst = PatchMapWithMove' $ PM.moveMapKey src dst

-- |Make a @'PatchMapWithMove' k v@ which has the effect of swapping two keys in the mapping, equivalent to:
--
-- @
--     let aMay = Map.lookup a map
--         bMay = Map.lookup b map
--     in maybe id (Map.insert a) (bMay `mplus` aMay)
--      . maybe id (Map.insert b) (aMay `mplus` bMay)
--      . Map.delete a . Map.delete b $ map
-- @
swapMapKey :: Ord k => k -> k -> PatchMapWithMove k v
swapMapKey src dst = PatchMapWithMove' $ PM.swapMapKey src dst

-- |Make a @'PatchMapWithMove' k v@ which has the effect of deleting a key in the mapping, equivalent to 'Map.delete'.
deleteMapKey :: k -> PatchMapWithMove k v
deleteMapKey = PatchMapWithMove' . PM.deleteMapKey

-- | Wrap a @'Map' k (NodeInfo k v)@ representing patch changes into a @'PatchMapWithMove' k v@, without checking any invariants.
--
-- __Warning:__ when using this function, you must ensure that the invariants of 'PatchMapWithMove' are preserved; they will not be checked.
unsafePatchMapWithMove :: Map k (NodeInfo k v) -> PatchMapWithMove k v
unsafePatchMapWithMove = PatchMapWithMove' . PM.unsafePatchMapWithPatchingMove

-- | Apply the insertions, deletions, and moves to a given 'Map'
instance Ord k => Patch (PatchMapWithMove k v) where
  type PatchTarget (PatchMapWithMove k v) = Map k v
  apply (PatchMapWithMove' p) old = apply p old

-- | Returns all the new elements that will be added to the 'Map'.
patchMapWithMoveNewElements :: PatchMapWithMove k v -> [v]
patchMapWithMoveNewElements = PM.patchMapWithPatchingMoveNewElements . unPatchMapWithMove'

-- | Return a @'Map' k v@ with all the inserts/updates from the given @'PatchMapWithMove' k v@.
patchMapWithMoveNewElementsMap :: PatchMapWithMove k v -> Map k v
patchMapWithMoveNewElementsMap = PM.patchMapWithPatchingMoveNewElementsMap . unPatchMapWithMove'

-- | Create a 'PatchMapWithMove' that, if applied to the given 'Map', will sort
-- its values using the given ordering function.  The set keys of the 'Map' is
-- not changed.
patchThatSortsMapWith :: Ord k => (v -> v -> Ordering) -> Map k v -> PatchMapWithMove k v
patchThatSortsMapWith cmp = PatchMapWithMove' . PM.patchThatSortsMapWith cmp

-- | Create a 'PatchMapWithMove' that, if applied to the first 'Map' provided,
-- will produce a 'Map' with the same values as the second 'Map' but with the
-- values sorted with the given ordering function.
patchThatChangesAndSortsMapWith :: forall k v. (Ord k, Ord v) => (v -> v -> Ordering) -> Map k v -> Map k v -> PatchMapWithMove k v
patchThatChangesAndSortsMapWith cmp oldByIndex newByIndexUnsorted = patchThatChangesMap oldByIndex newByIndex
  where newList = Map.toList newByIndexUnsorted
        newByIndex = Map.fromList $ zip (fst <$> newList) $ sortBy cmp $ snd <$> newList

-- | Create a 'PatchMapWithMove' that, if applied to the first 'Map' provided,
-- will produce the second 'Map'.
patchThatChangesMap :: (Ord k, Ord v) => Map k v -> Map k v -> PatchMapWithMove k v
patchThatChangesMap oldByIndex newByIndex = PatchMapWithMove' $
  PM.patchThatChangesMap oldByIndex newByIndex

-- | Change the 'From' value of a 'NodeInfo'
nodeInfoMapFrom :: (From k v -> From k v) -> NodeInfo k v -> NodeInfo k v
nodeInfoMapFrom = PM.nodeInfoMapFrom

-- | Change the 'From' value of a 'NodeInfo', using a 'Functor' (or
-- 'Applicative', 'Monad', etc.) action to get the new value
nodeInfoMapMFrom
  :: Functor f
  => (From k v -> f (From k v))
  -> NodeInfo k v -> f (NodeInfo k v)
nodeInfoMapMFrom = PM.nodeInfoMapMFrom

-- | Set the 'To' field of a 'NodeInfo'
nodeInfoSetTo :: To k -> NodeInfo k v -> NodeInfo k v
nodeInfoSetTo = PM.nodeInfoSetTo

makeWrapped ''PatchMapWithMove
