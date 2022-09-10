{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description: The module provides the 'Patch' class.

This is a class for types which represent changes made to other types
-}
module Data.Patch.Class where

import Data.Functor.Identity
import Data.Kind (Type)
import Data.Maybe
import Data.Semigroup
  ( Sum (..)
  , Product (..)
#if !MIN_VERSION_base(4,11,0)
  , Semigroup(..)
#endif
  )
import Data.Proxy

-- | A 'Patch' type represents a kind of change made to a datastructure.
--
-- If an instance of 'Patch' is also an instance of 'Semigroup', it should obey
-- the law that @applyAlways (f <> g) == applyAlways f . applyAlways g@.
class Patch p where
  type PatchTarget p :: Type
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)

-- | Apply a 'Patch'; if it does nothing, return the original value
applyAlways :: Patch p => p -> PatchTarget p -> PatchTarget p
applyAlways p t = fromMaybe t $ apply p t

-- | 'Identity' can be used as a 'Patch' that always fully replaces the value
instance Patch (Identity a) where
  type PatchTarget (Identity a) = a
  apply (Identity a) _ = Just a

-- | 'Proxy' can be used as a 'Patch' that does nothing.
instance forall (a :: Type). Patch (Proxy a) where
  type PatchTarget (Proxy a) = a
  apply ~Proxy _ = Nothing

instance (Num a, Eq a) => Patch (Sum a) where
  type PatchTarget (Sum a) = a
  apply (Sum a) b = if a == 0 then Nothing else Just $ a + b

instance (Num a, Eq a) => Patch (Product a) where
  type PatchTarget (Product a) = a
  apply (Product a) b = if a == 1 then Nothing else Just $ a * b

-- | Like '(.)', but composes functions that return patches rather than
-- functions that return new values.  The Semigroup instance for patches must
-- apply patches right-to-left, like '(.)'.
composePatchFunctions :: (Patch p, Semigroup p) => (PatchTarget p -> p) -> (PatchTarget p -> p) -> PatchTarget p -> p
composePatchFunctions g f a =
  let fp = f a
  in g (applyAlways fp a) <> fp
