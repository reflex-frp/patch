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

import Control.Applicative
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid.DecidablyEmpty
import Data.Semigroup (Semigroup (..), stimesIdempotentMonoid)
import Data.Map.Merge.Lazy

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
instance Ord k => Patch (PatchMap k v) where
  type PatchTarget (PatchMap k v) = Map k v
  {-# INLINABLE apply #-}
  apply (PatchMap p) old
    = changedToMaybe $
        mergeA
          (traverseMaybeMissing $ \_k mv ->
             case mv of
               Nothing -> Unchanged Nothing
               Just _ -> Changed mv)
          preserveMissing
          -- We could try to detect an update here that does nothing, but that
          -- will be quite unreliable for a map of Events or similar; it may
          -- not be worth the trouble.
          (zipWithMaybeAMatched (\_k mv v -> Changed $! mv <|> Just v)) p old

changedToMaybe :: Changed a -> Maybe a
changedToMaybe (Unchanged _) = Nothing
changedToMaybe (Changed a) = Just a

data Changed a
  = Unchanged a
  | Changed a
  deriving (Functor)

instance Applicative Changed where
  pure = Unchanged
#if MIN_VERSION_base(4,10,0)
  liftA2 f (Changed x) (Changed y) = Changed (f x y)
  liftA2 f (Unchanged x) (Changed y) = Changed (f x y)
  liftA2 f (Changed x) (Unchanged y) = Changed (f x y)
  liftA2 f (Unchanged x) (Unchanged y) = Unchanged (f x y)
#endif

instance FunctorWithIndex k (PatchMap k)
instance FoldableWithIndex k (PatchMap k)
instance TraversableWithIndex k (PatchMap k) where
  itraverse = itraversed . Indexed
  itraversed = _Wrapped .> itraversed <. traversed

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElements :: PatchMap k v -> [v]
patchMapNewElements (PatchMap p) = catMaybes $ Map.elems p

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElementsMap :: PatchMap k v -> Map k v
patchMapNewElementsMap (PatchMap p) = Map.mapMaybe id p

makeWrapped ''PatchMap
