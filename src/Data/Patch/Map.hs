{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description: A basic 'Patch' on 'Map'

Patches of this type consist only of insertions (including overwrites) and
deletions.
-}
module Data.Patch.Map where

import Data.Patch.Class

import Control.Lens hiding  (FunctorWithIndex, FoldableWithIndex, TraversableWithIndex)
#if !MIN_VERSION_lens(5,0,0)
import qualified Control.Lens as L
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid.DecidablyEmpty
import Data.Semigroup (Semigroup (..), stimesIdempotentMonoid)
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

-- | A set of changes to a 'Map'.  Any element may be inserted/updated or
-- deleted.  Insertions are represented as values wrapped in 'Just', while
-- deletions are represented as 'Nothing's
newtype PatchMap k v = PatchMap { unPatchMap :: Map k (Maybe v) }
  deriving ( Show, Read, Eq, Ord
           , Foldable, Traversable
           , DecidablyEmpty
           )

-- | 'fmap'ping a 'PatchMap' will alter all of the values it will insert.
-- Deletions are unaffected.
deriving instance Functor (PatchMap k)
-- | The empty 'PatchMap' contains no insertions or deletions
deriving instance Ord k => Monoid (PatchMap k v)

-- | @a <> b@ will apply the changes of @b@ and then apply the changes of @a@.
-- If the same key is modified by both patches, the one on the left will take
-- precedence.
instance Ord k => Semigroup (PatchMap k v) where
  PatchMap a <> PatchMap b = PatchMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
  stimes = stimesIdempotentMonoid

-- | Apply the insertions or deletions to a given 'Map'.
instance Ord k => PatchHet (PatchMap k v) where
  type PatchSource (PatchMap k v) = Map k v
  type PatchTarget (PatchMap k v) = Map k v
instance Ord k => Patch (PatchMap k v) where
  {-# INLINABLE apply #-}
  apply (PatchMap p) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = Map.mapMaybeWithKey (const id) p
          deletions = Map.mapMaybeWithKey (const nothingToJust) p
          nothingToJust = \case
            Nothing -> Just ()
            Just _ -> Nothing

makeWrapped ''PatchMap

instance FunctorWithIndex k (PatchMap k)
instance FoldableWithIndex k (PatchMap k)
instance TraversableWithIndex k (PatchMap k) where
  itraverse = (_Wrapped .> itraversed <. traversed) . Indexed

#if !MIN_VERSION_lens(5,0,0)
instance L.FunctorWithIndex k    (PatchMap k) where imap = Data.Functor.WithIndex.imap
instance L.FoldableWithIndex k   (PatchMap k) where ifoldMap = Data.Foldable.WithIndex.ifoldMap
instance L.TraversableWithIndex k (PatchMap k) where itraverse = Data.Traversable.WithIndex.itraverse
#endif

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElements :: PatchMap k v -> [v]
patchMapNewElements (PatchMap p) = catMaybes $ Map.elems p

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElementsMap :: PatchMap k v -> Map k v
patchMapNewElementsMap (PatchMap p) = Map.mapMaybe id p
