{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Patch.Patchable where

import Data.Patch
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

-- | Like SemiMap/PartialMap but for anything patchable
data Patchable p
  = Patchable_Patch p
  | Patchable_Complete (PatchTarget p)
  deriving (Generic)

deriving instance (Eq p, Eq (PatchTarget p)) => Eq (Patchable p)
deriving instance (Ord p, Ord (PatchTarget p)) => Ord (Patchable p)
deriving instance (Show p, Show (PatchTarget p)) => Show (Patchable p)
deriving instance (Read p, Read (PatchTarget p)) => Read (Patchable p)

completePatchable :: Patchable p -> Maybe (PatchTarget p)
completePatchable = \case
  Patchable_Complete t -> Just t
  Patchable_Patch _ -> Nothing

instance ( Monoid p
#if !MIN_VERSION_base(4,11,0)
         , Semigroup p
#endif
         , Patch p
         ) => Monoid (Patchable p) where
  mempty = Patchable_Patch mempty
  mappend = (<>)

instance (Semigroup p, Patch p) => Semigroup (Patchable p) where
  (<>) = curry $ \case
    (Patchable_Patch a, Patchable_Patch b) -> Patchable_Patch $ a <> b
    (Patchable_Patch a, Patchable_Complete b) -> Patchable_Complete $ applyAlways a b
    (Patchable_Complete a, _) -> Patchable_Complete a
