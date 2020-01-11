{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The interface for types which represent changes made to other types
module Data.Patch.Class where

import Data.Functor.Identity
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

-- | 'Identity' can be used as a 'Patch' that always fully replaces the value
instance PatchHet (Proxy a) where
  type PatchSource (Proxy a) = a
  type PatchTarget (Proxy a) = a
instance Patch (Proxy a) where
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
