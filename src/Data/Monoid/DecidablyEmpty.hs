{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- TODO upstream somwhere else?
module Data.Monoid.DecidablyEmpty where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Monoid
import Data.Maybe (isNothing)
#if MIN_VERSION_base(4,11,0)
import Data.Ord
#endif
import Data.Proxy
import Data.Semigroup hiding (First, Last)
#if MIN_VERSION_base(4,12,0)
import GHC.Generics
#endif

import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.GADT.Compare
import qualified Data.Dependent.Map as DMap

-- | A 'DecidablyEmpty' is one where it can be computed whether or not an
-- arbitrary value is 'mempty'.
--
-- By using this class rather than 'Eq', we avoid unnecessary constraining the
-- contents of 'Functor's. This makes it possible to efficiently combine and/or
-- nest patch maps with 'Eq'-lacking values (e.g. functions) at the leaves.
class Monoid a => DecidablyEmpty a where
  isEmpty :: a -> Bool
  default isEmpty :: Eq a => a -> Bool
  isEmpty = (==) mempty

-- base

instance DecidablyEmpty Ordering
instance DecidablyEmpty ()
instance DecidablyEmpty Any
instance DecidablyEmpty All
-- instance DecidablyEmpty Lifetime
-- instance DecidablyEmpty Event
instance DecidablyEmpty [a] where
  isEmpty = null
instance
#if MIN_VERSION_base(4,11,0)
  Semigroup a
#else
  Monoid a
#endif
  => DecidablyEmpty (Maybe a) where
  isEmpty = isNothing
deriving instance (Num a, DecidablyEmpty a) => DecidablyEmpty (Product a)
deriving instance (DecidablyEmpty a, Num a) => DecidablyEmpty (Sum a)
deriving instance DecidablyEmpty a => DecidablyEmpty (Dual a)
instance DecidablyEmpty (First a) where
  isEmpty (First a) = isNothing a
instance DecidablyEmpty (Last a) where
  isEmpty (Last a) = isNothing a
deriving instance DecidablyEmpty a => DecidablyEmpty (Identity a)
instance Semigroup a => DecidablyEmpty (Option a) where
  isEmpty (Option a) = isNothing a
deriving instance DecidablyEmpty m => DecidablyEmpty (WrappedMonoid m)
instance (Ord a, Bounded a) => DecidablyEmpty (Max a)
instance (Ord a, Bounded a) => DecidablyEmpty (Min a)
instance DecidablyEmpty (Proxy s)
deriving instance DecidablyEmpty a => DecidablyEmpty (Const a b)
#if MIN_VERSION_base(4,11,0)
deriving instance DecidablyEmpty a => DecidablyEmpty (Down a)
#endif
#if MIN_VERSION_base(4,12,0)
deriving instance DecidablyEmpty p => DecidablyEmpty (Par1 p)
instance DecidablyEmpty (U1 p)
deriving instance DecidablyEmpty (f p) => DecidablyEmpty (Rec1 f p)
deriving instance DecidablyEmpty (f p) => DecidablyEmpty (M1 i c f p)
deriving instance DecidablyEmpty c => DecidablyEmpty (K1 i c p)
instance (DecidablyEmpty (f p), DecidablyEmpty (g p)) => DecidablyEmpty ((f :*: g) p) where
  isEmpty (x :*: y) = isEmpty x && isEmpty y
deriving instance DecidablyEmpty (f (g p)) => DecidablyEmpty ((f :.: g) p)
#endif

instance (DecidablyEmpty a, DecidablyEmpty b) => DecidablyEmpty (a, b) where
  isEmpty (a, b) = isEmpty a && isEmpty b
instance (DecidablyEmpty a, DecidablyEmpty b, DecidablyEmpty c) => DecidablyEmpty (a, b, c) where
  isEmpty (a, b, c) = isEmpty a && isEmpty b && isEmpty c
instance (DecidablyEmpty a, DecidablyEmpty b, DecidablyEmpty c, DecidablyEmpty d) => DecidablyEmpty (a, b, c, d) where
  isEmpty (a, b, c, d) = isEmpty a && isEmpty b && isEmpty c && isEmpty d
instance (DecidablyEmpty a, DecidablyEmpty b, DecidablyEmpty c, DecidablyEmpty d, DecidablyEmpty e) => DecidablyEmpty (a, b, c, d, e) where
  isEmpty (a, b, c, d, e) = isEmpty a && isEmpty b && isEmpty c && isEmpty d && isEmpty e

-- containers

instance DecidablyEmpty IntSet.IntSet where
  isEmpty = IntSet.null
instance DecidablyEmpty (IntMap.IntMap v) where
  isEmpty = IntMap.null
instance Ord k => DecidablyEmpty (Map.Map k v) where
  isEmpty = Map.null
instance DecidablyEmpty (Seq.Seq v) where
  isEmpty = Seq.null
instance Ord k => DecidablyEmpty (Set.Set k) where
  isEmpty = Set.null

-- dependent-map

instance GCompare k => DecidablyEmpty (DMap.DMap k v) where
  isEmpty = DMap.null
