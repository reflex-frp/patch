{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The interface for types which represent changes made to other types
module Data.Patch.Class where

import qualified Data.Semigroupoid as Cat
import Data.Functor.Identity
import Data.Functor.Misc
import Data.Maybe
import Data.Semigroup (Semigroup(..))
import Data.Proxy
import Data.Type.Equality ((:~:) (..))

class PatchHet p where
  type PatchSource p :: *
  type PatchTarget p :: *
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

-- | 'Proxy' can be used as a 'Patch' that always fully replaces the value
instance PatchHet (Proxy (a :: *)) where
  type PatchSource (Proxy a) = a
  type PatchTarget (Proxy a) = a
instance Patch (Proxy (a :: *)) where
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


class PatchHet2Base (p :: k -> k -> *) where
  type PatchSource1 p :: k -> *
  type PatchTarget1 p :: k -> *

class ( PatchHet2Base p
      , PatchHet (p from to)
      , PatchSource1 p from ~ PatchSource (p from to)
      , PatchTarget1 p to ~ PatchTarget (p from to)
      ) => PatchHet2Locally (p :: k -> k -> *) from to where
instance ( PatchHet2Base p
         , PatchHet (p from to)
         , PatchSource1 p from ~ PatchSource (p from to)
         , PatchTarget1 p to ~ PatchTarget (p from to)
         ) => PatchHet2Locally (p :: k -> k -> *) from to where

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
class PatchHet2Base p => PatchHet2 (p :: k -> k -> *) where
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
instance PatchHet (First2 (t :: k -> *) (from :: k) (to :: k)) where
  type PatchSource (First2 t from to) = t from
  type PatchTarget (First2 t from to) = t to
  applyHet (First2 val) _ = Right val

-- | 'Proxy3' can be used as a 'Patch' that always does nothing
instance PatchHet (Proxy3 (t :: k -> *) (a :: k) (a :: k)) where
  type PatchSource (Proxy3 t a a) = t a
  type PatchTarget (Proxy3 t a a) = t a
  applyHet ~Proxy3 _ = Left Refl

instance PatchHet2Base (Proxy3 (t :: k -> *) :: k -> k -> *) where
  type PatchSource1 (Proxy3 t) = t
  type PatchTarget1 (Proxy3 t) = t
