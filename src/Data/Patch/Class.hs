{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
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

class (forall x y
       . ( PatchHet (p x y)
         , PatchSource1 p x ~ PatchSource (p x y)
         , PatchTarget1 p y ~ PatchTarget (p x y)
         )
      ) => PatchHet2 (p :: k -> k -> *) where
  type PatchSource1 p :: k -> *
  type PatchTarget1 p :: k -> *

class ( PatchHet2 p
      , PatchSource1 p ~ PatchTarget1 p
      ) => Patch2 p
instance ( PatchHet2 p
         , PatchSource1 p ~ PatchTarget1 p
         ) => Patch2 p

newtype Replace2 (t :: k -> *) (a :: k) (b :: k) = Replace2 (t b)
  deriving ( Show, Read, Eq, Ord
           , Functor, Foldable, Traversable
           )

data Proxy2 t a b = Proxy2
  deriving ( Show, Read, Eq, Ord
           , Functor, Foldable, Traversable
           )

-- | 'Replace2' can be used as a 'Patch' that always fully replaces the value
instance PatchHet (Replace2 (t :: k -> *) (a :: k) (b :: k)) where
  type PatchSource (Replace2 t a b) = t a
  type PatchTarget (Replace2 t a b) = t b
  applyHet (Replace2 val) _ = Right val

-- | 'Proxy2' can be used as a 'Patch' that always fully replaces the value
instance PatchHet (Proxy2 (t :: k -> *) (a :: k) (a :: k)) where
  type PatchSource (Proxy2 t a a) = t a
  type PatchTarget (Proxy2 t a a) = t a
  applyHet ~Proxy2 _ = Left Refl
