{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Patch.PatchOrReplacement where

import Data.Patch
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

-- | Like SemiMap/PartialMap but for anything patchable
data PatchOrReplacement p
  = PatchOrReplacement_Patch p
  | PatchOrReplacement_Complete (PatchTarget p)
  deriving (Generic)

deriving instance (Eq p, Eq (PatchTarget p)) => Eq (PatchOrReplacement p)
deriving instance (Ord p, Ord (PatchTarget p)) => Ord (PatchOrReplacement p)
deriving instance (Show p, Show (PatchTarget p)) => Show (PatchOrReplacement p)
deriving instance (Read p, Read (PatchTarget p)) => Read (PatchOrReplacement p)

completePatchOrReplacement :: PatchOrReplacement p -> Maybe (PatchTarget p)
completePatchOrReplacement = \case
  PatchOrReplacement_Complete t -> Just t
  PatchOrReplacement_Patch _ -> Nothing

instance ( Monoid p
#if !MIN_VERSION_base(4,11,0)
         , Semigroup p
#endif
         , Patch p
         ) => Monoid (PatchOrReplacement p) where
  mempty = PatchOrReplacement_Patch mempty
  mappend = (<>)

instance (Semigroup p, Patch p) => Semigroup (PatchOrReplacement p) where
  (<>) = curry $ \case
    (PatchOrReplacement_Patch a, PatchOrReplacement_Patch b) -> PatchOrReplacement_Patch $ a <> b
    (PatchOrReplacement_Patch a, PatchOrReplacement_Complete b) -> PatchOrReplacement_Complete $ applyAlways a b
    (PatchOrReplacement_Complete a, _) -> PatchOrReplacement_Complete a
