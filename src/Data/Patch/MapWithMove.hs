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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Description: An intermediate 'Patch' on 'Map'

Patches of this type can insert, delete, and also move values from one key to
another.
-}
module Data.Patch.MapWithMove
  ( PatchMapWithMove
    ( PatchMapWithMove
    , unPatchMapWithMove
    , ..
    )
  , patchMapWithMove
  , patchMapWithMoveInsertAll
  , insertMapKey
  , moveMapKey
  , swapMapKey
  , deleteMapKey
  , unsafePatchMapWithMove
  , patchMapWithMoveNewElements
  , patchMapWithMoveNewElementsMap
  , patchThatSortsMapWith
  , patchThatChangesAndSortsMapWith
  , patchThatChangesMap

  -- * Node Info
  , NodeInfo
    ( NodeInfo
    , _nodeInfo_from
    , _nodeInfo_to
    , ..
    )
  , bitraverseNodeInfo
  , nodeInfoMapFrom
  , nodeInfoMapMFrom
  , nodeInfoSetTo

  -- * From
  , From
    ( From_Insert
    , From_Delete
    , From_Move
    , ..
    )
  , bitraverseFrom

  -- * To
  , To
  ) where

import Data.Coerce
import Data.Kind (Type)
import Data.Patch.Class
import Data.Patch.MapWithPatchingMove (PatchMapWithPatchingMove(..), To)
import qualified Data.Patch.MapWithPatchingMove as PM -- already a transparent synonym

import Control.Lens hiding  (FunctorWithIndex, FoldableWithIndex, TraversableWithIndex)
import qualified Control.Lens as L
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import Data.Traversable (foldMapDefault)
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

-- | Patch a Map with additions, deletions, and moves.  Invariant: If key @k1@
-- is coming from @From_Move k2@, then key @k2@ should be going to @Just k1@,
-- and vice versa.  There should never be any unpaired From/To keys.
newtype PatchMapWithMove k (v :: Type) = PatchMapWithMove'
  { -- | Extract the underlying 'PatchMapWithPatchingMove k (Proxy v)'
    unPatchMapWithMove' :: PatchMapWithPatchingMove k (Proxy v)
  }
  deriving ( Show, Read, Eq, Ord
-- Haddock cannot handle documentation here before GHC 8.6
           ,
#if __GLASGOW_HASKELL__ >= 806
             -- | Compose patches having the same effect as applying the
             -- patches in turn: @'applyAlways' (p <> q) == 'applyAlways' p .
             -- 'applyAlways' q@
#endif
             Semigroup
           , Monoid
           )

pattern Coerce :: Coercible a b => a -> b
pattern Coerce x <- (coerce -> x)
  where Coerce x = coerce x

{-# COMPLETE PatchMapWithMove #-}
pattern PatchMapWithMove :: Map k (NodeInfo k v) -> PatchMapWithMove k v
-- | Extract the representation of the 'PatchMapWithMove' as a map of
-- 'NodeInfo'.
unPatchMapWithMove :: PatchMapWithMove k v -> Map k (NodeInfo k v)
pattern PatchMapWithMove { unPatchMapWithMove } = PatchMapWithMove' (PatchMapWithPatchingMove (Coerce unPatchMapWithMove))

_PatchMapWithMove
  :: Iso
       (PatchMapWithMove k0 v0)
       (PatchMapWithMove k1 v1)
       (Map k0 (NodeInfo k0 v0))
       (Map k1 (NodeInfo k1 v1))
_PatchMapWithMove = iso unPatchMapWithMove PatchMapWithMove

instance Functor (PatchMapWithMove k) where
  fmap f = runIdentity . traverse (Identity . f)

instance Foldable (PatchMapWithMove k) where
  foldMap = foldMapDefault

instance Traversable (PatchMapWithMove k) where
  traverse =
    _PatchMapWithMove .
    traverse .
    traverse

instance FunctorWithIndex k (PatchMapWithMove k)
instance FoldableWithIndex k (PatchMapWithMove k)
instance TraversableWithIndex k (PatchMapWithMove k) where
  itraverse = (_PatchMapWithMove .> itraversed <. traverse) . Indexed

#if !MIN_VERSION_lens(5,0,0)
instance L.FunctorWithIndex k    (PatchMapWithMove k) where imap = Data.Functor.WithIndex.imap
instance L.FoldableWithIndex k   (PatchMapWithMove k) where ifoldMap = Data.Foldable.WithIndex.ifoldMap
instance L.TraversableWithIndex k (PatchMapWithMove k) where itraverse = Data.Traversable.WithIndex.itraverse
#endif

-- | Create a 'PatchMapWithMove', validating it
patchMapWithMove :: Ord k => Map k (NodeInfo k v) -> Maybe (PatchMapWithMove k v)
patchMapWithMove = fmap PatchMapWithMove' . PM.patchMapWithPatchingMove . coerce

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
--     in maybe id (Map.insert a) (bMay <> aMay)
--      . maybe id (Map.insert b) (aMay <> bMay)
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
unsafePatchMapWithMove = coerce PM.unsafePatchMapWithPatchingMove

-- | Apply the insertions, deletions, and moves to a given 'Map'
instance Ord k => Patch (PatchMapWithMove k v) where
  type PatchTarget (PatchMapWithMove k v) = Map k v
  apply (PatchMapWithMove' p) = apply p

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
patchThatChangesAndSortsMapWith :: (Ord k, Ord v) => (v -> v -> Ordering) -> Map k v -> Map k v -> PatchMapWithMove k v
patchThatChangesAndSortsMapWith cmp oldByIndex newByIndexUnsorted = patchThatChangesMap oldByIndex newByIndex
  where newList = Map.toList newByIndexUnsorted
        newByIndex = Map.fromList $ zip (fst <$> newList) $ sortBy cmp $ snd <$> newList

-- | Create a 'PatchMapWithMove' that, if applied to the first 'Map' provided,
-- will produce the second 'Map'.
patchThatChangesMap :: (Ord k, Ord v) => Map k v -> Map k v -> PatchMapWithMove k v
patchThatChangesMap oldByIndex newByIndex = PatchMapWithMove' $
  PM.patchThatChangesMap oldByIndex newByIndex

--
-- NodeInfo
--

-- | Holds the information about each key: where its new value should come from,
-- and where its old value should go to
newtype NodeInfo k (v :: Type) = NodeInfo' { unNodeInfo' :: PM.NodeInfo k (Proxy v) }

deriving instance (Show k, Show p) => Show (NodeInfo k p)
deriving instance (Read k, Read p) => Read (NodeInfo k p)
deriving instance (Eq k, Eq p) => Eq (NodeInfo k p)
deriving instance (Ord k, Ord p) => Ord (NodeInfo k p)

{-# COMPLETE NodeInfo #-}
pattern NodeInfo :: From k v -> To k -> NodeInfo k v
_nodeInfo_from :: NodeInfo k v -> From k v
_nodeInfo_to :: NodeInfo k v -> To k
pattern NodeInfo { _nodeInfo_from, _nodeInfo_to } = NodeInfo'
  PM.NodeInfo
    { PM._nodeInfo_from = Coerce _nodeInfo_from
    , PM._nodeInfo_to = _nodeInfo_to
    }

_NodeInfo
  :: Iso
       (NodeInfo k0 v0)
       (NodeInfo k1 v1)
       (PM.NodeInfo k0 (Proxy v0))
       (PM.NodeInfo k1 (Proxy v1))
_NodeInfo = iso unNodeInfo' NodeInfo'

instance Functor (NodeInfo k) where
  fmap f = runIdentity . traverse (Identity . f)

instance Foldable (NodeInfo k) where
  foldMap = foldMapDefault

instance Traversable (NodeInfo k) where
  traverse = bitraverseNodeInfo pure

bitraverseNodeInfo
  :: Applicative f
  => (k0 -> f k1)
  -> (v0 -> f v1)
  -> NodeInfo k0 v0 -> f (NodeInfo k1 v1)
bitraverseNodeInfo fk fv = fmap NodeInfo'
  . PM.bitraverseNodeInfo fk (\ ~Proxy -> pure Proxy) fv
  . coerce

-- | Change the 'From' value of a 'NodeInfo'
nodeInfoMapFrom :: (From k v -> From k v) -> NodeInfo k v -> NodeInfo k v
nodeInfoMapFrom f = coerce $ PM.nodeInfoMapFrom (unFrom' . f . From')

-- | Change the 'From' value of a 'NodeInfo', using a 'Functor' (or
-- 'Applicative', 'Monad', etc.) action to get the new value
nodeInfoMapMFrom
  :: Functor f
  => (From k v -> f (From k v))
  -> NodeInfo k v -> f (NodeInfo k v)
nodeInfoMapMFrom f = fmap NodeInfo'
  . PM.nodeInfoMapMFrom (fmap unFrom' . f . From')
  . coerce

-- | Set the 'To' field of a 'NodeInfo'
nodeInfoSetTo :: To k -> NodeInfo k v -> NodeInfo k v
nodeInfoSetTo = coerce . PM.nodeInfoSetTo

--
-- From
--

-- | Describe how a key's new value should be produced
newtype From k (v :: Type) = From' { unFrom' :: PM.From k (Proxy v) }

{-# COMPLETE From_Insert, From_Delete, From_Move #-}

-- | Insert the given value here
pattern From_Insert :: v -> From k v
pattern From_Insert v = From' (PM.From_Insert v)

-- | Delete the existing value, if any, from here
pattern From_Delete :: From k v
pattern From_Delete = From' PM.From_Delete

-- | Move the value here from the given key
pattern From_Move :: k -> From k v
pattern From_Move k = From' (PM.From_Move k Proxy)

bitraverseFrom
  :: Applicative f
  => (k0 -> f k1)
  -> (v0 -> f v1)
  -> From k0 v0 -> f (From k1 v1)
bitraverseFrom fk fv = fmap From'
  . PM.bitraverseFrom fk (\ ~Proxy -> pure Proxy) fv
  . coerce

makeWrapped ''PatchMapWithMove
makeWrapped ''NodeInfo
makeWrapped ''From
