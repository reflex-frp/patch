{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description: Module containing 'PatchIntMap', a 'Patch' for 'IntMap'.

Patches of this sort allow for insert/update or delete of associations.
-}
module Data.Patch.IntMap where

import Control.Lens hiding  (FunctorWithIndex, FoldableWithIndex, TraversableWithIndex)
#if !MIN_VERSION_lens(5,0,0)
import qualified Control.Lens as L
#endif
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Monoid.DecidablyEmpty
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import Data.Patch.Class
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

-- | 'Patch' for 'IntMap' which represents insertion or deletion of keys in the mapping.
-- Internally represented by 'IntMap (Maybe a)', where @Just@ means insert/update
-- and @Nothing@ means delete.
newtype PatchIntMap a = PatchIntMap { unPatchIntMap :: IntMap (Maybe a) }
  deriving ( Show, Read, Eq, Ord
           , Functor, Foldable, Traversable
           , Monoid, DecidablyEmpty
           )

-- | @a <> b@ will apply the changes of @b@ and then apply the changes of @a@.
-- If the same key is modified by both patches, the one on the left will take
-- precedence.
deriving instance Semigroup (PatchIntMap v)

makeWrapped ''PatchIntMap

-- | Apply the insertions or deletions to a given 'IntMap'.
instance PatchHet (PatchIntMap a) where
  type PatchSource (PatchIntMap a) = IntMap a
  type PatchTarget (PatchIntMap a) = IntMap a
instance Patch (PatchIntMap a) where
  apply (PatchIntMap p) v = if IntMap.null p then Nothing else Just $
    let removes = IntMap.filter isNothing p
        adds = IntMap.mapMaybe id p
    in IntMap.union adds $ v `IntMap.difference` removes

instance FunctorWithIndex Int PatchIntMap
instance FoldableWithIndex Int PatchIntMap
instance TraversableWithIndex Int PatchIntMap where
  itraverse = (_Wrapped .> itraversed <. traversed) . Indexed

#if !MIN_VERSION_lens(5,0,0)
instance L.FunctorWithIndex     Int PatchIntMap where imap = Data.Functor.WithIndex.imap
instance L.FoldableWithIndex    Int PatchIntMap where ifoldMap = Data.Foldable.WithIndex.ifoldMap
instance L.TraversableWithIndex Int PatchIntMap where itraverse = Data.Traversable.WithIndex.itraverse
#endif

-- | Map a function @Int -> a -> b@ over all @a@s in the given @'PatchIntMap' a@
-- (that is, all inserts/updates), producing a @PatchIntMap b@.
mapIntMapPatchWithKey :: (Int -> a -> b) -> PatchIntMap a -> PatchIntMap b
mapIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap $ IntMap.mapWithKey (\ k mv -> f k <$> mv) m

-- | Map an effectful function @Int -> a -> f b@ over all @a@s in the given @'PatchIntMap' a@
-- (that is, all inserts/updates), producing a @f (PatchIntMap b)@.
traverseIntMapPatchWithKey :: Applicative f => (Int -> a -> f b) -> PatchIntMap a -> f (PatchIntMap b)
traverseIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap <$> IntMap.traverseWithKey (traverse . f) m

-- | Extract all @a@s inserted/updated by the given @'PatchIntMap' a@.
patchIntMapNewElements :: PatchIntMap a -> [a]
patchIntMapNewElements (PatchIntMap m) = catMaybes $ IntMap.elems m

-- | Convert the given @'PatchIntMap' a@ into an @'IntMap' a@ with all
-- the inserts/updates in the given patch.
patchIntMapNewElementsMap :: PatchIntMap a -> IntMap a
patchIntMapNewElementsMap (PatchIntMap m) = IntMap.mapMaybe id m

-- | Subset the given @'IntMap' a@ to contain only the keys that would be
-- deleted by the given @'PatchIntMap' a@.
getDeletions :: PatchIntMap v -> IntMap v' -> IntMap v'
getDeletions (PatchIntMap m) v = IntMap.intersection v m
