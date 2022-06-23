{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Description:
  This module defines the 'Group' class, and reexports the other modules.
-}
module Data.Patch
  ( module Data.Patch
  , module X
  ) where

import Data.Semigroup.Commutative
import Control.Applicative (liftA2)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity
import Data.Map.Monoidal (MonoidalMap)
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
import GHC.Generics

import qualified Data.Semigroup.Additive as X
import Data.Patch.Class as X
import Data.Patch.DMap as X hiding (getDeletions)
import Data.Patch.DMapWithMove as X
  ( PatchDMapWithMove, const2PatchDMapWithMoveWith, mapPatchDMapWithMove
  , patchDMapWithMoveToPatchMapWithMoveWith
  , traversePatchDMapWithMoveWithKey, unPatchDMapWithMove
  , unsafePatchDMapWithMove, weakenPatchDMapWithMoveWith
  )
import Data.Patch.DMapWithPatchingMove as X
  ( PatchDMapWithPatchingMove, const2PatchDMapWithPatchingMoveWith, mapPatchDMapWithPatchingMove
  , patchDMapWithPatchingMoveToPatchMapWithPatchingMoveWith
  , traversePatchDMapWithPatchingMoveWithKey, unPatchDMapWithPatchingMove
  , unsafePatchDMapWithPatchingMove, weakenPatchDMapWithPatchingMoveWith
  )
import Data.Patch.IntMap as X hiding (getDeletions)
import Data.Patch.Map as X
import Data.Patch.MapWithMove as X
  ( PatchMapWithMove, patchMapWithMoveNewElements
  , patchMapWithMoveNewElementsMap, unPatchMapWithMove
  , unsafePatchMapWithMove
  )

-- | A 'Group' is a 'Monoid' where every element has an inverse.
class (Semigroup q, Monoid q) => Group q where
  negateG :: q -> q
  (~~) :: q -> q -> q
  r ~~ s = r <> negateG s

-- | The elements of an 'Commutative' 'Semigroup' can be considered as patches of their own type.
newtype AdditivePatch p = AdditivePatch { unAdditivePatch :: p }

instance Commutative p => PatchHet (AdditivePatch p) where
  type PatchSource (AdditivePatch p) = p
  type PatchTarget (AdditivePatch p) = p
instance Commutative p => Patch (AdditivePatch p) where
  apply (AdditivePatch p) q = Just $ p <> q

instance (Ord k, Group q) => Group (MonoidalMap k q) where
  negateG = fmap negateG

-- | Trivial group.
instance Group () where
  negateG ~() = ()
  ~() ~~ ~() = ()

-- | Product group.  A Pair of groups gives rise to a group
instance (Group a, Group b) => Group (a, b) where
  negateG (a, b) = (negateG a, negateG b)
  (a, b) ~~ (c, d) = (a ~~ c, b ~~ d)

-- See https://gitlab.haskell.org/ghc/ghc/issues/11135#note_111802 for the reason Compose is not also provided.
-- Base does not define Monoid (Compose f g a) so this is the best we can
-- really do for functor composition.
instance Group (f (g a)) => Group ((f :.: g) a) where
  negateG (Comp1 xs) = Comp1 (negateG xs)
  Comp1 xs ~~ Comp1 ys = Comp1 (xs ~~ ys)

-- | Product of groups, Functor style.
instance (Group (f a), Group (g a)) => Group ((f :*: g) a) where
  negateG (a :*: b) = negateG a :*: negateG b
  (a :*: b) ~~ (c :*: d) = (a ~~ c) :*: (b ~~ d)

-- | Trivial group, Functor style
instance Group (Proxy x) where
  negateG ~Proxy = Proxy
  ~Proxy ~~ ~Proxy = Proxy

-- | Const lifts groups into a functor.
deriving instance Group a => Group (Const a x)

-- | Identity lifts groups pointwise (at only one point)
deriving instance Group a => Group (Identity a)

-- | Functions lift groups pointwise.
instance Group b => Group (a -> b) where
  negateG f = negateG . f
  (~~) = liftA2 (~~)
