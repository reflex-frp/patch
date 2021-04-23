{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Description: The module provides the 'Patch' class.

This is a class for types which represent changes made to other types
-}
module Data.Patch.Class where

import qualified Data.Semigroupoid as Cat
import qualified Control.Category as Cat
import Data.Functor.Identity
import Data.Functor.Misc
import Data.Kind (Type)
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Proxy
import Data.Typeable

class PatchHet p where
  type PatchSource p :: Type
  type PatchTarget p :: Type
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  applyHet
    :: p
    -> PatchSource p
    -> Either (PatchSource p :~: PatchTarget p) (PatchTarget p)
  default applyHet
    :: Patch p
    => p
    -> PatchSource p
    -> Either (PatchSource p :~: PatchTarget p) (PatchTarget p)
  applyHet p a = case apply p a of
    Nothing -> Left Refl
    Just a' -> Right a'

-- | Apply a 'PatchHet'; if it does nothing, return the original value
applyAlwaysHet :: PatchHet p => p -> PatchSource p -> PatchTarget p
applyAlwaysHet p t = case applyHet p t of
  Left Refl -> t
  Right t' -> t'

-- | A 'Patch' type represents a kind of change made to a datastructure.
--
-- If an instance of 'Patch' is also an instance of 'Semigroup', it should obey
-- the law that @applyAlways (f <> g) == applyAlways f . applyAlways g@.
class ( PatchHet p
      , PatchSource p ~ PatchTarget p
      ) => Patch p where
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)

-- | Apply a 'Patch'; if it does nothing, return the original value
applyAlways :: Patch p => p -> PatchTarget p -> PatchTarget p
applyAlways p t = fromMaybe t $ apply p t

-- | 'Identity' can be used as a 'Patch' that always fully replaces the value
instance PatchHet (Identity a) where
  type PatchSource (Identity a) = a
  type PatchTarget (Identity a) = a
instance Patch (Identity a) where
  apply (Identity a) _ = Just a

-- | 'Proxy' can be used as a 'Patch' that does nothing.
instance forall (a :: Type). PatchHet (Proxy a) where
  type PatchSource (Proxy a) = a
  type PatchTarget (Proxy a) = a
instance forall (a :: Type). Patch (Proxy a) where
  apply ~Proxy _ = Nothing

-- | Like '(.)', but composes functions that return patches rather than
-- functions that return new values.  The Semigroup instance for patches must
-- apply patches right-to-left, like '(.)'.
composePatchFunctions
  :: (Patch p, Semigroup p)
  => (PatchTarget p -> p)
  -> (PatchTarget p -> p)
  -> PatchTarget p -> p
composePatchFunctions g f a =
  let fp = f a
  in g (applyAlways fp a) <> fp


class PatchHet2Base (p :: k -> k -> Type) where
  type PatchSource1 p :: k -> Type
  type PatchTarget1 p :: k -> Type

class ( PatchHet2Base p
      , PatchHet (p from to)
      , PatchSource1 p from ~ PatchSource (p from to)
      , PatchTarget1 p to ~ PatchTarget (p from to)
      ) => PatchHet2Locally (p :: k -> k -> Type) from to where
instance ( PatchHet2Base p
         , PatchHet (p from to)
         , PatchSource1 p from ~ PatchSource (p from to)
         , PatchTarget1 p to ~ PatchTarget (p from to)
         ) => PatchHet2Locally (p :: k -> k -> Type) from to where

applyHet2Locally
  :: PatchHet2Locally p from to
  => p from to
  -> PatchSource1 p from
  -> Either (PatchSource1 p from :~: PatchTarget1 p to) (PatchTarget1 p to)
applyHet2Locally = applyHet

applyAlwaysHet2Locally
  :: PatchHet2Locally p from to
  => p from to
  -> PatchSource1 p from
  -> PatchTarget1 p to
applyAlwaysHet2Locally = applyAlwaysHet

-- TODO once we can use quantified constraints, perhaps combine PatchHet2Base and
-- PatchHet2Locally, or at least get rid of this.
class PatchHet2Base p => PatchHet2 (p :: k -> k -> Type) where
  applyHet2
    :: p from to
    -> PatchSource1 p from
    -> Either (PatchSource1 p from :~: PatchTarget1 p to) (PatchTarget1 p to)

applyAlwaysHet2
  :: PatchHet2 p
  => p from to
  -> PatchSource1 p from
  -> PatchTarget1 p to
applyAlwaysHet2 p t = case applyHet2 p t of
  Left Refl -> t
  Right t' -> t'

-- | Connect the classes without quanitified constraints
newtype ProjectLocal p from to = ProjectLocal { unProjectLocal :: p from to }
  deriving newtype Cat.Semigroupoid

instance PatchHet2 p => PatchHet (ProjectLocal p from to) where
  type PatchSource (ProjectLocal p from to) = PatchSource1 p from
  type PatchTarget (ProjectLocal p from to) = PatchTarget1 p to
  applyHet (ProjectLocal p) = applyHet2 p

instance PatchHet2 p => PatchHet2Base (ProjectLocal p) where
  type PatchSource1 (ProjectLocal p) = PatchSource1 p
  type PatchTarget1 (ProjectLocal p) = PatchTarget1 p

class ( PatchHet2Base p
      , PatchSource1 p ~ PatchTarget1 p
      ) => Patch2 p
instance ( PatchHet2Base p
         , PatchSource1 p ~ PatchTarget1 p
         ) => Patch2 p

-- | 'First2' can be used as a 'Patch' that always fully replaces the value
instance PatchHet (First2 (t :: k -> Type) (from :: k) (to :: k)) where
  type PatchSource (First2 t from to) = t from
  type PatchTarget (First2 t from to) = t to
  applyHet (First2 val) _ = Right val

data IndexedEq :: (k -> Type) -> k -> k -> Type where
  IndexedRefl :: IndexedEq k x x
  deriving (Typeable)

deriving instance Eq (IndexedEq k x y)
deriving instance Ord (IndexedEq k x y)
deriving instance Show (IndexedEq k x y)
deriving instance Read (IndexedEq k x x)

instance Cat.Category (IndexedEq x) where
  id = IndexedRefl
  IndexedRefl . IndexedRefl = IndexedRefl

-- | 'IndexedEq' can be used as a 'Patch' that always does nothing
instance PatchHet (IndexedEq (t :: k -> Type) (a :: k) (b :: k)) where
  type PatchSource (IndexedEq t a b) = t a
  type PatchTarget (IndexedEq t a b) = t b
  applyHet IndexedRefl _ = Left Refl

instance PatchHet2Base (IndexedEq (t :: k -> Type) :: k -> k -> Type) where
  type PatchSource1 (IndexedEq t) = t
  type PatchTarget1 (IndexedEq t) = t
