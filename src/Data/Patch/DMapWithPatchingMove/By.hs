{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Patch.DMapWithPatchingMove.By where

import Data.Kind (Type)
import Data.Semigroupoid as Cat

import Data.Patch.Class

-- | Structure describing a particular change to a key, be it inserting a new
-- key (@By_Insert@), updating an existing key (@By_Insert@ again), deleting
-- a key (@By_Delete@), or moving a key (@By_Move@).
--
-- This type isn't used directly as the from field patch, but is instead wrapped
-- in an existential. However, it is nice to be able to reason about this in
-- isolation as it is itself a @Semigroupoid@ when the underlying patch is.
data By (k :: a -> Type) (p :: a -> a -> Type) :: a -> a -> Type where
  -- | Insert a new or update an existing key with the given value @PatchTarget1
  -- p a@
  By_Insert :: PatchTarget1 p to -> By k p from to
  -- | Delete the existing key
  By_Delete :: By k p from to
  -- | Move the value from the given key @k a@ to this key. The source key
  -- should also have an entry in the patch giving the current key as
  -- @_nodeInfo_to@, usually but not necessarily with @By_Delete@.
  By_Move :: !(k from) -> p from to -> By k p from to

deriving instance ( Show (k from), Show (k to)
                  , Show (p from to)
                  , Show (PatchTarget1 p to)
                  ) => Show (By k p from to)
deriving instance ( Read (k from), Read (k to)
                  , Read (p from to)
                  , Read (PatchTarget1 p to)
                  ) => Read (By k p from to)
deriving instance ( Eq (k from), Eq (k to)
                  , Eq (p from to)
                  , Eq (PatchTarget1 p to)
                  ) => Eq (By k p from to)
deriving instance ( Ord (k from), Ord (k to)
                  , Ord (p from to)
                  , Ord (PatchTarget1 p to)
                  ) => Ord (By k p from to)

mapByPatch
  :: PatchTarget1 p0 ~ PatchTarget1 p1
  => (p0 from to -> p1 from to)
  -> By k p0 from to
  -> By k p1 from to
mapByPatch f = \case
  By_Insert v -> By_Insert v
  By_Delete -> By_Delete
  By_Move k p -> By_Move k $ f p

-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance ( PatchSource1 p ~ PatchTarget1 p
         , Cat.Semigroupoid p
         , PatchHet2 p
         ) => Cat.Semigroupoid (By k p) where
  o p0 p1 = mapByPatch unProjectLocal $
    oLocal (mapByPatch ProjectLocal p0) (mapByPatch ProjectLocal p1)

oLocal
  :: ( PatchSource1 p ~ PatchTarget1 p
     , PatchHet2Locally p between after
     , Cat.Semigroupoid p
     )
  => By k p between after
  -> By k p before between
  -> By k p before after
By_Insert new `oLocal` _ = By_Insert new
By_Delete `oLocal` _ = By_Delete
By_Move _ x `oLocal` By_Insert y = By_Insert $ applyAlwaysHet2Locally x y
By_Move _ x `oLocal` By_Move src y = By_Move src $ x `o` y
By_Move _ _ `oLocal` By_Delete = By_Delete
