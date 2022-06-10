{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Patch.PatchOrReplacement where

import Data.Patch
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

-- | Either a patch or a replacement value.
--
-- A good patch type will describe small changes very efficiently, but
-- that often comes at the cost of describing large change rather
-- inefficiently. 'PatchOrReplacement' can be used as an escape hatch:
-- when the change as a patch would be too big, just provide a new value
-- to replace the old one with instead.
data PatchOrReplacement p
  = PatchOrReplacement_Patch p
  | PatchOrReplacement_Replacement (PatchTarget p)
  deriving (Generic)

deriving instance (Eq p, Eq (PatchTarget p)) => Eq (PatchOrReplacement p)
deriving instance (Ord p, Ord (PatchTarget p)) => Ord (PatchOrReplacement p)
deriving instance (Show p, Show (PatchTarget p)) => Show (PatchOrReplacement p)
deriving instance (Read p, Read (PatchTarget p)) => Read (PatchOrReplacement p)

completePatchOrReplacement :: PatchOrReplacement p -> Maybe (PatchTarget p)
completePatchOrReplacement = \case
  PatchOrReplacement_Replacement t -> Just t
  PatchOrReplacement_Patch _ -> Nothing

-- | 'PatchOrReplacement p' is a patch when we can apply the patch or
-- replace the old value with the new replacement value.
instance Patch p => Patch (PatchOrReplacement p) where
  type PatchTarget (PatchOrReplacement p) = PatchTarget p
  apply = \case
    PatchOrReplacement_Patch p -> apply p
    PatchOrReplacement_Replacement v -> \_ -> Just v

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
    (PatchOrReplacement_Patch a, PatchOrReplacement_Replacement b) -> PatchOrReplacement_Replacement $ applyAlways a b
    (PatchOrReplacement_Replacement a, _) -> PatchOrReplacement_Replacement a
