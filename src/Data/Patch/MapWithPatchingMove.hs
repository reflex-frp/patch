{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description: An advanced 'Patch' on 'Map'

Patches of this type can can insert, delete, and move values from one key to
another, and move patches may also additionally patch the value being moved.
-}
module Data.Patch.MapWithPatchingMove
  ( PatchMapWithPatchingMove (..)
  , patchMapWithPatchingMove
  , patchMapWithPatchingMoveInsertAll
  , insertMapKey
  , moveMapKey
  , swapMapKey
  , deleteMapKey
  , unsafePatchMapWithPatchingMove
  , patchMapWithPatchingMoveNewElements
  , patchMapWithPatchingMoveNewElementsMap
  , patchThatSortsMapWith
  , patchThatChangesAndSortsMapWith
  , patchThatChangesMap

  -- * Node Info
  , NodeInfo (..)
  , bitraverseNodeInfo
  , nodeInfoMapFrom
  , nodeInfoMapMFrom
  , nodeInfoSetTo

  -- * From
  , From(..)
  , bitraverseFrom

  -- * To
  , To

  -- TODO internals module
  , Fixup (..)
  ) where

import Data.Patch.Class

import Control.Lens ((<&>))
import Control.Lens.TH (makeWrapped)
import Data.Align (align)
import Data.Foldable (toList)
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import Data.Monoid.DecidablyEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These (..))

-- | Patch a Map with additions, deletions, and moves.  Invariant: If key @k1@
-- is coming from @From_Move k2@, then key @k2@ should be going to @Just k1@,
-- and vice versa.  There should never be any unpaired From/To keys.
newtype PatchMapWithPatchingMove k p = PatchMapWithPatchingMove
  { -- | Extract the internal representation of the 'PatchMapWithPatchingMove'
    unPatchMapWithPatchingMove :: Map k (NodeInfo k p)
  }

deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (PatchMapWithPatchingMove k p)
deriving instance (Ord k, Read k, Read p, Read (PatchTarget p)) => Read (PatchMapWithPatchingMove k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (PatchMapWithPatchingMove k p)
deriving instance (Ord k, Ord p, Ord (PatchTarget p)) => Ord (PatchMapWithPatchingMove k p)

deriving instance ( Ord k
#if !MIN_VERSION_base(4,11,0)
                  , Semigroup p
#endif
                  , DecidablyEmpty p
                  , Patch p
                  ) => DecidablyEmpty (PatchMapWithPatchingMove k p)

-- | Create a 'PatchMapWithPatchingMove', validating it
patchMapWithPatchingMove
  :: Ord k => Map k (NodeInfo k p) -> Maybe (PatchMapWithPatchingMove k p)
patchMapWithPatchingMove m = if valid then Just $ PatchMapWithPatchingMove m else Nothing
  where valid = forwardLinks == backwardLinks
        forwardLinks = Map.mapMaybe _nodeInfo_to m
        backwardLinks = Map.fromList $ catMaybes $ flip fmap (Map.toList m) $ \(to, p) ->
          case _nodeInfo_from p of
            From_Move from _ -> Just (from, to)
            _ -> Nothing

-- | Create a 'PatchMapWithPatchingMove' that inserts everything in the given 'Map'
patchMapWithPatchingMoveInsertAll
  :: Map k (PatchTarget p) -> PatchMapWithPatchingMove k p
patchMapWithPatchingMoveInsertAll m = PatchMapWithPatchingMove $ flip fmap m $ \v -> NodeInfo
  { _nodeInfo_from = From_Insert v
  , _nodeInfo_to = Nothing
  }

-- | Make a @'PatchMapWithPatchingMove' k p@ which has the effect of inserting or replacing a value @v@ at the given key @k@, like 'Map.insert'.
insertMapKey
  :: k -> PatchTarget p -> PatchMapWithPatchingMove k p
insertMapKey k v = PatchMapWithPatchingMove . Map.singleton k $ NodeInfo (From_Insert v) Nothing

-- |Make a @'PatchMapWithPatchingMove' k p@ which has the effect of moving the value from the first key @k@ to the second key @k@, equivalent to:
--
-- @
--     'Map.delete' src (maybe map ('Map.insert' dst) (Map.lookup src map))
-- @
moveMapKey
  :: ( DecidablyEmpty p
#if !MIN_VERSION_base(4,11,0)
     , Semigroup p
#endif
     , Patch p
     )
  => Ord k => k -> k -> PatchMapWithPatchingMove k p
moveMapKey src dst
  | src == dst = mempty
  | otherwise =
      PatchMapWithPatchingMove $ Map.fromList
        [ (dst, NodeInfo (From_Move src mempty) Nothing)
        , (src, NodeInfo From_Delete (Just dst))
        ]

-- |Make a @'PatchMapWithPatchingMove' k p@ which has the effect of swapping two keys in the mapping, equivalent to:
--
-- @
--     let aMay = Map.lookup a map
--         bMay = Map.lookup b map
--     in maybe id (Map.insert a) (bMay <> aMay)
--      . maybe id (Map.insert b) (aMay <> bMay)
--      . Map.delete a . Map.delete b $ map
-- @
swapMapKey
  :: ( DecidablyEmpty p
#if !MIN_VERSION_base(4,11,0)
     , Semigroup p
#endif
     , Patch p
     )
  => Ord k => k -> k -> PatchMapWithPatchingMove k p
swapMapKey src dst
  | src == dst = mempty
  | otherwise =
    PatchMapWithPatchingMove $ Map.fromList
      [ (dst, NodeInfo (From_Move src mempty) (Just src))
      , (src, NodeInfo (From_Move dst mempty) (Just dst))
      ]

-- | Make a @'PatchMapWithPatchingMove' k v@ which has the effect of deleting a key in
-- the mapping, equivalent to 'Map.delete'.
deleteMapKey
  :: k -> PatchMapWithPatchingMove k v
deleteMapKey k = PatchMapWithPatchingMove . Map.singleton k $ NodeInfo From_Delete Nothing

-- | Wrap a @'Map' k (NodeInfo k v)@ representing patch changes into a @'PatchMapWithPatchingMove' k v@, without checking any invariants.
--
-- __Warning:__ when using this function, you must ensure that the invariants of 'PatchMapWithPatchingMove' are preserved; they will not be checked.
unsafePatchMapWithPatchingMove
  :: Map k (NodeInfo k p) -> PatchMapWithPatchingMove k p
unsafePatchMapWithPatchingMove = PatchMapWithPatchingMove

-- | Apply the insertions, deletions, and moves to a given 'Map'
instance (Ord k, Patch p) => PatchHet (PatchMapWithPatchingMove k p) where
  type PatchSource (PatchMapWithPatchingMove k p) = Map k (PatchSource p)
  type PatchTarget (PatchMapWithPatchingMove k p) = Map k (PatchTarget p)

instance (Ord k, Patch p) => Patch (PatchMapWithPatchingMove k p) where
  -- TODO: return Nothing sometimes
  -- Note: the strict application here is critical to ensuring that incremental
  -- merges don't hold onto all their prerequisite events forever; can we make
  -- this more robust?
  apply (PatchMapWithPatchingMove m) old = Just $! insertions `Map.union` (old `Map.difference` deletions)
    where insertions = flip Map.mapMaybeWithKey m $ \_ ni -> case _nodeInfo_from ni of
            From_Insert v -> Just v
            From_Move k p -> applyAlways p <$> Map.lookup k old
            From_Delete -> Nothing
          deletions = flip Map.mapMaybeWithKey m $ \_ ni -> case _nodeInfo_from ni of
            From_Delete -> Just ()
            _ -> Nothing

-- | Returns all the new elements that will be added to the 'Map'
patchMapWithPatchingMoveNewElements
  :: PatchMapWithPatchingMove k p -> [PatchTarget p]
patchMapWithPatchingMoveNewElements = Map.elems . patchMapWithPatchingMoveNewElementsMap

-- | Return a @'Map' k v@ with all the inserts/updates from the given @'PatchMapWithPatchingMove' k v@.
patchMapWithPatchingMoveNewElementsMap
  :: PatchMapWithPatchingMove k p -> Map k (PatchTarget p)
patchMapWithPatchingMoveNewElementsMap (PatchMapWithPatchingMove p) = Map.mapMaybe f p
  where f ni = case _nodeInfo_from ni of
          From_Insert v -> Just v
          From_Move _ _ -> Nothing
          From_Delete -> Nothing

-- | Create a 'PatchMapWithPatchingMove' that, if applied to the given 'Map', will sort
-- its values using the given ordering function.  The set keys of the 'Map' is
-- not changed.
patchThatSortsMapWith
  :: (Ord k, Monoid p)
  => (PatchTarget p -> PatchTarget p -> Ordering)
  -> Map k (PatchTarget p) -> PatchMapWithPatchingMove k p
patchThatSortsMapWith cmp m = PatchMapWithPatchingMove $ Map.fromList $ catMaybes $ zipWith g unsorted sorted
  where unsorted = Map.toList m
        sorted = sortBy (cmp `on` snd) unsorted
        f (to, _) (from, _) = if to == from then Nothing else
          Just (from, to)
        reverseMapping = Map.fromList $ catMaybes $ zipWith f unsorted sorted
        g (to, _) (from, _) = if to == from then Nothing else
          let Just movingTo = Map.lookup from reverseMapping
          in Just (to, NodeInfo (From_Move from mempty) $ Just movingTo)

-- | Create a 'PatchMapWithPatchingMove' that, if applied to the first 'Map' provided,
-- will produce a 'Map' with the same values as the second 'Map' but with the
-- values sorted with the given ordering function.
patchThatChangesAndSortsMapWith
  :: forall k p. (Ord k, Ord (PatchTarget p), Monoid p)
  => (PatchTarget p -> PatchTarget p -> Ordering)
  -> Map k (PatchTarget p) -> Map k (PatchTarget p) -> PatchMapWithPatchingMove k p
patchThatChangesAndSortsMapWith cmp oldByIndex newByIndexUnsorted = patchThatChangesMap oldByIndex newByIndex
  where newList = Map.toList newByIndexUnsorted
        newByIndex = Map.fromList $ zip (fst <$> newList) $ sortBy cmp $ snd <$> newList

-- | Create a 'PatchMapWithPatchingMove' that, if applied to the first 'Map' provided,
-- will produce the second 'Map'.
-- Note: this will never produce a patch on a value.
patchThatChangesMap
  :: forall k p
  .  (Ord k, Ord (PatchTarget p), Monoid p)
  => Map k (PatchTarget p) -> Map k (PatchTarget p) -> PatchMapWithPatchingMove k p
patchThatChangesMap oldByIndex newByIndex = patch
  where invert :: Map k (PatchTarget p) -> Map (PatchTarget p) (Set k)
        invert = Map.fromListWith (<>) . fmap (\(k, v) -> (v, Set.singleton k)) . Map.toList
        -- In the places where we use unionDistinct, a non-distinct key indicates a bug in this function
        unionDistinct :: forall k' v'. Ord k' => Map k' v' -> Map k' v' -> Map k' v'
        unionDistinct = Map.unionWith (error "patchThatChangesMap: non-distinct keys")
        unionPairDistinct :: (Map k (From k v), Map k (To k)) -> (Map k (From k v), Map k (To k)) -> (Map k (From k v), Map k (To k))
        unionPairDistinct (oldFroms, oldTos) (newFroms, newTos) = (unionDistinct oldFroms newFroms, unionDistinct oldTos newTos)
        -- Generate patch info for a single value
        -- Keys that are found in both the old and new sets will not be patched
        -- Keys that are found in only the old set will be moved to a new position if any are available; otherwise they will be deleted
        -- Keys that are found in only the new set will be populated by moving an old key if any are available; otherwise they will be inserted
        patchSingleValue :: PatchTarget p -> Set k -> Set k -> (Map k (From k p), Map k (To k))
        patchSingleValue v oldKeys newKeys = foldl' unionPairDistinct mempty $ align (toList $ oldKeys `Set.difference` newKeys) (toList $ newKeys `Set.difference` oldKeys) <&> \case
          This oldK -> (mempty, Map.singleton oldK Nothing) -- There's nowhere for this value to go, so we know we are deleting it
          That newK -> (Map.singleton newK $ From_Insert v, mempty) -- There's nowhere fo this value to come from, so we know we are inserting it
          These oldK newK -> (Map.singleton newK $ From_Move oldK mempty, Map.singleton oldK $ Just newK)
        -- Run patchSingleValue on a These.  Missing old or new sets are considered empty
        patchSingleValueThese :: PatchTarget p -> These (Set k) (Set k) -> (Map k (From k p), Map k (To k))
        patchSingleValueThese v = \case
          This oldKeys -> patchSingleValue v oldKeys mempty
          That newKeys -> patchSingleValue v mempty newKeys
          These oldKeys newKeys -> patchSingleValue v oldKeys newKeys
        -- Generate froms and tos for all values, then merge them together
        (froms, tos) = foldl' unionPairDistinct mempty $ Map.mapWithKey patchSingleValueThese $ align (invert oldByIndex) (invert newByIndex)
        patch = unsafePatchMapWithPatchingMove $ align froms tos <&> \case
          This from -> NodeInfo from Nothing -- Since we don't have a 'to' record for this key, that must mean it isn't being moved anywhere, so it should be deleted.
          That to -> NodeInfo From_Delete to -- Since we don't have a 'from' record for this key, it must be getting deleted
          These from to -> NodeInfo from to

--
-- NodeInfo
--

-- | Holds the information about each key: where its new value should come from,
-- and where its old value should go to
data NodeInfo k p = NodeInfo
  { _nodeInfo_from :: !(From k p)
    -- ^ Where do we get the new value for this key?
  , _nodeInfo_to :: !(To k)
    -- ^ If the old value is being kept (i.e. moved rather than deleted or
    -- replaced), where is it going?
  }
deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (NodeInfo k p)
deriving instance (Read k, Read p, Read (PatchTarget p)) => Read (NodeInfo k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (NodeInfo k p)
deriving instance (Ord k, Ord p, Ord (PatchTarget p)) => Ord (NodeInfo k p)

-- | Traverse the 'NodeInfo' over the key, patch, and patch target. Because of
-- the type families here, this doesn't it any bi- or tri-traversal class.
bitraverseNodeInfo
  :: Applicative f
  => (k0 -> f k1)
  -> (p0 -> f p1)
  -> (PatchTarget p0 -> f (PatchTarget p1))
  -> NodeInfo k0 p0 -> f (NodeInfo k1 p1)
bitraverseNodeInfo fk fp fpt (NodeInfo from to) = NodeInfo
  <$> bitraverseFrom fk fp fpt from
  <*> traverse fk to

-- | Change the 'From' value of a 'NodeInfo'
nodeInfoMapFrom
  :: (From k v -> From k v) -> NodeInfo k v -> NodeInfo k v
nodeInfoMapFrom f ni = ni { _nodeInfo_from = f $ _nodeInfo_from ni }

-- | Change the 'From' value of a 'NodeInfo', using a 'Functor' (or
-- 'Applicative', 'Monad', etc.) action to get the new value
nodeInfoMapMFrom
  :: Functor f => (From k v -> f (From k v)) -> NodeInfo k v -> f (NodeInfo k v)
nodeInfoMapMFrom f ni = fmap (\result -> ni { _nodeInfo_from = result }) $ f $ _nodeInfo_from ni

-- | Set the 'To' field of a 'NodeInfo'
nodeInfoSetTo
  :: To k -> NodeInfo k v -> NodeInfo k v
nodeInfoSetTo to ni = ni { _nodeInfo_to = to }

--
-- From
--

-- | Describe how a key's new value should be produced
data From k p
   = From_Insert (PatchTarget p) -- ^ Insert the given value here
   | From_Delete -- ^ Delete the existing value, if any, from here
   | From_Move !k !p -- ^ Move the value here from the given key, and apply the given patch

deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (From k p)
deriving instance (Read k, Read p, Read (PatchTarget p)) => Read (From k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (From k p)
deriving instance (Ord k, Ord p, Ord (PatchTarget p)) => Ord (From k p)

-- | Traverse the 'From' over the key, patch, and patch target. Because of
-- the type families here, this doesn't it any bi- or tri-traversal class.
bitraverseFrom
  :: Applicative f
  => (k0 -> f k1)
  -> (p0 -> f p1)
  -> (PatchTarget p0 -> f (PatchTarget p1))
  -> From k0 p0 -> f (From k1 p1)
bitraverseFrom fk fp fpt = \case
  From_Insert pt -> From_Insert <$> fpt pt
  From_Delete -> pure From_Delete
  From_Move k p -> From_Move <$> fk k <*> fp p

--
-- To
--

-- | Describe where a key's old value will go.  If this is 'Just', that means
-- the key's old value will be moved to the given other key; if it is 'Nothing',
-- that means it will be deleted.
type To = Maybe

--
-- Fixup
--

-- | Helper data structure used for composing patches using the monoid instance.
data Fixup k v
   = Fixup_Delete
   | Fixup_Update (These (From k v) (To k))

-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance ( Ord k
#if !MIN_VERSION_base(4,11,0)
         , Semigroup p
#endif
         , DecidablyEmpty p
         , Patch p
         ) => Semigroup (PatchMapWithPatchingMove k p) where
  PatchMapWithPatchingMove ma <> PatchMapWithPatchingMove mb = PatchMapWithPatchingMove m
    where
      connections = Map.toList $ Map.intersectionWithKey (\_ a b -> (_nodeInfo_to a, _nodeInfo_from b)) ma mb
      h :: (k, (Maybe k, From k p)) -> [(k, Fixup k p)]
      h (_, (mToAfter, editBefore)) = case (mToAfter, editBefore) of
        (Just toAfter, From_Move fromBefore p)
          | fromBefore == toAfter && isEmpty p
            -> [(toAfter, Fixup_Delete)]
          | otherwise
            -> [ (toAfter, Fixup_Update (This editBefore))
               , (fromBefore, Fixup_Update (That mToAfter))
               ]
        (Nothing, From_Move fromBefore _) ->
           -- The item is destroyed in the second patch, so indicate that it is
           -- destroyed in the source map
           [(fromBefore, Fixup_Update (That mToAfter))]
        (Just toAfter, _) -> [(toAfter, Fixup_Update (This editBefore))]
        (Nothing, _) -> []
      mergeFixups _ Fixup_Delete Fixup_Delete = Fixup_Delete
      mergeFixups _ (Fixup_Update a) (Fixup_Update b)
        | This x <- a, That y <- b
        = Fixup_Update $ These x y
        | That y <- a, This x <- b
        = Fixup_Update $ These x y
      mergeFixups _ _ _ = error "PatchMapWithPatchingMove: incompatible fixups"
      fixups = Map.fromListWithKey mergeFixups $ concatMap h connections
      combineNodeInfos _ nia nib = NodeInfo
        { _nodeInfo_from = _nodeInfo_from nia
        , _nodeInfo_to = _nodeInfo_to nib
        }
      applyFixup _ ni = \case
        Fixup_Delete -> Nothing
        Fixup_Update u -> Just $ NodeInfo
          { _nodeInfo_from = case _nodeInfo_from ni of
              f@(From_Move _ p') -> case getHere u of -- The `from` fixup comes from the "old" patch
                Nothing -> f -- If there's no `from` fixup, just use the "new" `from`
                Just (From_Insert v) -> From_Insert $ applyAlways p' v
                Just From_Delete -> From_Delete
                Just (From_Move oldKey p) -> From_Move oldKey $ p' <> p
              _ -> error "PatchMapWithPatchingMove: fixup for non-move From"
          , _nodeInfo_to = fromMaybe (_nodeInfo_to ni) $ getThere u
          }
      m = Map.differenceWithKey applyFixup (Map.unionWithKey combineNodeInfos ma mb) fixups
      getHere :: These a b -> Maybe a
      getHere = \case
        This a -> Just a
        These a _ -> Just a
        That _ -> Nothing
      getThere :: These a b -> Maybe b
      getThere = \case
        This _ -> Nothing
        These _ b -> Just b
        That b -> Just b

--TODO: Figure out how to implement this in terms of PatchDMapWithPatchingMove rather than duplicating it here
-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance ( Ord k
#if !MIN_VERSION_base(4,11,0)
         , Semigroup p
#endif
         , DecidablyEmpty p
         , Patch p
         ) => Monoid (PatchMapWithPatchingMove k p) where
  mempty = PatchMapWithPatchingMove mempty
  mappend = (<>)

makeWrapped ''PatchMapWithPatchingMove
