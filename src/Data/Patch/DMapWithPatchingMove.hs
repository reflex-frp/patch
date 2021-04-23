{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |Module containing @'PatchDMapWithPatchingMove' k v@ and associated functions, which represents a 'Patch' to a @'DMap' k v@ which can insert, update, delete, and
-- move values between keys.
module Data.Patch.DMapWithPatchingMove where

import qualified Control.Category as Cat
--import qualified Control.Category.DecidablyEmpty as Cat

import Data.Constraint.Extras (Has')
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Constant (Constant (..))
import Data.Functor.Misc
  ( Const2 (..), Proxy3 (..)
  , weakenDMapWith
  , dmapToMapWith
  )
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import Data.GADT.Compare (GEq (..), GCompare (..))
import Data.GADT.Show (GRead, GShow, gshow)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid.DecidablyEmpty
import Data.Semigroupoid as Cat
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..), (<>))
#endif
import Data.Some (Some (Some), mkSome)
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import Data.Type.Equality ((:~:)(..))

import Data.Patch.Class
  ( Patch (..), PatchHet (..)
  , PatchHet2 (..), PatchSource1, PatchTarget1
  , applyAlwaysHet2
  )
import Data.Patch.MapWithPatchingMove (PatchMapWithPatchingMove (..))
import qualified Data.Patch.MapWithPatchingMove as MapWithPatchingMove

-- | Like 'PatchMapWithPatchingMove', but for 'DMap'. Each key carries a 'NodeInfo'
-- which describes how it will be changed by the patch and connects move sources
-- and destinations.
--
-- Invariants:
--
--  * A key should not move to itself.
--
--  * A move should always be represented with both the destination key (as a
--    'From_Move') and the source key (as a @'ComposeMaybe' ('Just'
--    destination)@)
newtype PatchDMapWithPatchingMove k v = PatchDMapWithPatchingMove (DMap k (NodeInfo k v))

--deriving instance ( GShow k
--                  , HasZip Show k p
--                  , Has' Show k (PatchTarget1 p)
--                  ) => Show (PatchDMapWithPatchingMove k p)
--deriving instance ( GRead k
--                  , HasZip Read k p
--                  , Has' Read k (PatchTarget1 p)
--                  ) => Read (PatchDMapWithPatchingMove k p)
--deriving instance ( GEq k
--                  , HasZip Eq k p
--                  , Has' Eq k (PatchTarget1 p)
--                  ) => Eq (PatchDMapWithPatchingMove k p)
--deriving instance ( GCompare k
--                  , HasZip Ord k p
--                  , Has' Ord k (PatchTarget1 p)
--                  ) => Ord (PatchDMapWithPatchingMove k p)

-- It won't let me derive for some reason
instance ( GCompare k
         , Cat.Semigroupoid v
         , PatchHet2 v
         , PatchSource1 v ~ PatchTarget1 v
         ) => DecidablyEmpty (PatchDMapWithPatchingMove k v) where
  isEmpty (PatchDMapWithPatchingMove m) = DMap.null m

-- | Structure which represents what changes apply to a particular key.
-- @_nodeInfo_from@ specifies what happens to this key, and in particular what
-- other key the current key is moving from, while @_nodeInfo_to@ specifies what
-- key the current key is moving to if involved in a move.
data NodeInfo k p a = NodeInfo
  { _nodeInfo_from :: !(From k p a)
  -- ^ Change applying to the current key, be it an insert, move, or delete.
  , _nodeInfo_to :: !(To k)
  -- ^ Where this key is moving to, if involved in a move. Should only be
  -- @ComposeMaybe (Just k)@ when there is a corresponding 'From_Move'.
  }

--deriving instance ( Show (k a)
--                  , Show (p a a)
--                  , Show (PatchTarget1 p a)
--                  ) => Show (NodeInfo k p a)
--deriving instance ( Read (k a)
--                  , Read (p a a)
--                  , Read (PatchTarget1 p a)
--                  ) => Read (NodeInfo k p a)
--deriving instance ( Eq (k a)
--                  , Eq (p a a)
--                  , Eq (PatchTarget1 p a)
--                  ) => Eq (NodeInfo k p a)
--deriving instance ( Ord (k a)
--                  , Ord (p a a)
--                  , Ord (PatchTarget1 p a)
--                  ) => Ord (NodeInfo k p a)

-- | Structure describing a particular change to a key, be it inserting a new
-- key (@From_Insert@), updating an existing key (@From_Insert@ again), deleting
-- a key (@From_Delete@), or moving a key (@From_Move@).
--
-- This type isn't used directly as the from field patch, but is instead wrapped
-- in an existential. However, it is nice to be able to reason about this in
-- isolation as it is itself a @Semigroupoid@ when the underlying patch is.
data From (k :: a -> *) (p :: a -> a -> *) :: a -> * where
  -- | Insert a new or update an existing key with the given value @PatchTarget1
  -- p a@
  From_Insert :: PatchTarget1 p to -> From k p to
  -- | Delete the existing key
  From_Delete :: From k p to
  -- | Move the value from the given key @k a@ to this key. The source key
  -- should also have an entry in the patch giving the current key as
  -- @_nodeInfo_to@, usually but not necessarily with @From_Delete@.
  From_Move :: !(DSum k (Flip p to)) -> From k p to

deriving instance ( Show (k a), GShow k
                  , Has' Show k (Flip p a)
                  , Show (PatchTarget1 p a)
                  ) => Show (From k p a)
deriving instance ( Read (k a), GRead k
                  , Has' Read k (Flip p a)
                  , Read (PatchTarget1 p a)
                  ) => Read (From k p a)
deriving instance ( GEq k
                  , Has' Eq k (Flip p a)
                  , Eq (PatchTarget1 p a)
                  ) => Eq (From k p a)
deriving instance ( GCompare k
                  , Has' Eq k (Flip p a) -- superclass bug
                  , Has' Ord k (Flip p a)
                  , Ord (PatchTarget1 p a)
                  ) => Ord (From k p a)

newtype Flip p to from = Flip (p from to)

instance Cat.Category p => Cat.Category (Flip (p :: k -> k -> *)) where
  id = Flip Cat.id
  Flip y . Flip x = Flip $ x Cat.. y

-- | The "to" part of a 'NodeInfo'. Rather than be built out of @From@ like @From@
-- is, we store just the information necessary to compose a @To@ and @From@ like
-- @oLocal@ composes two @From@s.
data To (k :: a -> *) where
  -- | Delete or leave in place
  To_NonMove :: To k
  -- | Move the value from the given key @k a@ to this key. The target key
  -- should also have an entry in the patch giving the current key in
  -- @_nodeInfo_from@, usually but not necessarily with @To_Delete@.
  To_Move :: !(Some k) -> To k

deriving instance GShow k => Show (To k)
deriving instance GRead k => Read (To k)
deriving instance GEq k => Eq (To k)
deriving instance GCompare k => Ord (To k)

-- |Test whether a 'PatchDMapWithPatchingMove' satisfies its invariants.
validPatchDMapWithPatchingMove
  :: forall k v
  .  (GCompare k, GShow k)
  => DMap k (NodeInfo k v)
  -> Bool
validPatchDMapWithPatchingMove = not . null . validationErrorsForPatchDMapWithPatchingMove

-- |Enumerate what reasons a 'PatchDMapWithPatchingMove' doesn't satisfy its invariants, returning @[]@ if it's valid.
validationErrorsForPatchDMapWithPatchingMove
  :: forall k v
  .  (GCompare k, GShow k)
  => DMap k (NodeInfo k v)
  -> [String]
validationErrorsForPatchDMapWithPatchingMove m =
  noSelfMoves <> movesBalanced
  where
    noSelfMoves = mapMaybe selfMove . DMap.toAscList $ m
    selfMove (dst :=> NodeInfo (From_Move (src :=> _)) _)
      | Just _ <- dst `geq` src = Just $ "self move of key " <> gshow src <> " at destination side"
    selfMove (src :=> NodeInfo _ (To_Move (Some dst)))
      | Just _ <- src `geq` dst = Just $ "self move of key " <> gshow dst <> " at source side"
    selfMove _ = Nothing
    movesBalanced = mapMaybe unbalancedMove . DMap.toAscList $ m
    unbalancedMove (dst :=> NodeInfo (From_Move (src :=> _)) _) =
      case DMap.lookup src m of
        Nothing -> Just $ "unbalanced move at destination key " <> gshow dst <> " supposedly from " <> gshow src <> " but source key is not in the patch"
        Just (NodeInfo _ (To_Move (Some dst'))) ->
          if isNothing (dst' `geq` dst)
            then Just $ "unbalanced move at destination key " <> gshow dst <> " from " <> gshow src <> " is going to " <> gshow dst' <> " instead"
            else Nothing
        _ ->
          Just $ "unbalanced move at destination key " <> gshow dst <> " supposedly from " <> gshow src <> " but source key has no move to key"
    unbalancedMove (src :=> NodeInfo _ (To_Move (Some dst))) =
      case DMap.lookup dst m of
        Nothing -> Just $ " unbalanced move at source key " <> gshow src <> " supposedly going to " <> gshow dst <> " but destination key is not in the patch"
        Just (NodeInfo (From_Move (src' :=> _)) _) ->
          if isNothing (src' `geq` src)
            then Just $ "unbalanced move at source key " <> gshow src <> " to " <> gshow dst <> " is coming from " <> gshow src' <> " instead"
            else Nothing

        _ ->
          Just $ "unbalanced move at source key " <> gshow src <> " supposedly going to " <> gshow dst <> " but destination key is not moving"
    unbalancedMove _ = Nothing


data ToFrom k p a = ToFrom (To k) (From k p a)

-- | Helper data structure used for composing patches using the monoid instance.
data Fixup k p a
   = Fixup_Delete
   | Fixup_Update (These (DSum k (From k p)) (To k))

-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance forall k p
         . ( GCompare k
           , Cat.Semigroupoid p
           -- , Cat.DecidablyEmpty p
           , PatchHet2 p
           , PatchSource1 p ~ PatchTarget1 p
           )
         => Semigroup (PatchDMapWithPatchingMove k p) where
  PatchDMapWithPatchingMove ma <> PatchDMapWithPatchingMove mb = PatchDMapWithPatchingMove m
    where
      connections :: [DSum k (ToFrom k p)]
      connections = DMap.toList $ DMap.intersectionWithKey
        (\_ a b -> ToFrom (_nodeInfo_to a) (_nodeInfo_from b))
        ma
        mb
      h :: DSum k (ToFrom k p) -> [DSum k (Fixup k p)]
      h ((between :: k between) :=> ToFrom editAfter editBefore) = case (editAfter, editBefore) of
        (To_Move (Some (toAfter :: k after)), From_Move ((fromBefore :: k before) :=> Flip p) :: From k p between) ->
          --case toAfter `geq` fromBefore of
          --  Just Refl | Just Refl <- Cat.isId p0 ->
          --    [ toAfter :=> Fixup_Delete ]
          --  _ ->
              [ toAfter :=> Fixup_Update (This $ between :=> From_Move (fromBefore :=> Flip p))
              , fromBefore :=> Fixup_Update (That $ To_Move $ Some toAfter)
              ]
        (To_NonMove, From_Move (fromBefore :=> _)) ->
          -- The item is destroyed in the second patch, so indicate that it is
          -- destroyed in the source map
          [fromBefore :=> Fixup_Update (That To_NonMove)]
        --(To_Move (Some toAfter), From_Insert val) ->
        --  [toAfter :=> Fixup_Update (This $ From_Insert $ applyAlwaysHet2 p val)]
        --(To_Move (Some toAfter), From_Delete) ->
        --  [toAfter :=> Fixup_Update (This From_Delete)]
        (To_Move (Some toAfter), _) ->
          [toAfter :=> Fixup_Update (This $ between :=> editBefore)]
        (To_NonMove, _) ->
          []
      mergeFixups _ Fixup_Delete Fixup_Delete = Fixup_Delete
      mergeFixups _ (Fixup_Update a) (Fixup_Update b)
        | This x <- a, That y <- b
        = Fixup_Update $ These x y
        | That y <- a, This x <- b
        = Fixup_Update $ These x y
      mergeFixups _ _ _ = error "PatchDMapWithPatchingMove: incompatible fixups"
      fixups = DMap.fromListWithKey mergeFixups $ concatMap h connections
      combineNodeInfos _ nia nib = NodeInfo
        { _nodeInfo_from = _nodeInfo_from nia
        , _nodeInfo_to = _nodeInfo_to nib
        }
      applyFixup :: k a -> NodeInfo k p a -> Fixup k p a -> Maybe (NodeInfo k p a)
      applyFixup _ ni = \case
        Fixup_Delete -> Nothing
        Fixup_Update u -> Just $ NodeInfo
          { _nodeInfo_from = case _nodeInfo_from ni of
              f@(From_Move ((between0 :: k between0) :=> Flip (p' :: p between0 a))) -> case getHere u of -- The `from` fixup comes from the "old" patch
                Nothing -> f -- If there's no `from` fixup, just use the "new" `from`
                Just ((between1 :: k between1) :=> frm) -> case geq between0 between1 of
                  Nothing -> error "fixup joined-on key did not match"
                  Just Refl -> case frm of
                    From_Insert v -> From_Insert $ applyAlwaysHet2 p' v
                    From_Delete -> From_Delete
                    From_Move (oldKey :=> Flip (p :: p oldKey between1)) -> From_Move $ oldKey :=> Flip (p' `o` p :: p oldKey a)
              _ -> error "PatchMapWithPatchingMove: fixup for non-move From"
          , _nodeInfo_to = fromMaybe (_nodeInfo_to ni) $ getThere u
          }
      m = DMap.differenceWithKey applyFixup (DMap.unionWithKey combineNodeInfos ma mb) fixups
      getHere :: These a b -> Maybe a
      getHere = \case
        This a -> Just a
        These a _ -> Just a
        That _ -> Nothing
      getThere :: These a b -> Maybe b
      getThere = \case
        This _ -> Nothing
        These _ b -> Just b
        That b -> Just b

-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance ( GCompare k
         , Cat.Semigroupoid p
         -- , DecidablyEmpty p
         , PatchHet2 p
         , PatchSource1 p ~ PatchTarget1 p
         ) => Monoid (PatchDMapWithPatchingMove k p) where
  mempty = PatchDMapWithPatchingMove mempty
  mappend = (<>)

{-
mappendPatchDMapWithPatchingMoveSlow :: forall k v.  (ShowTag k v, GCompare k) => PatchDMapWithPatchingMove k v -> PatchDMapWithPatchingMove k v -> PatchDMapWithPatchingMove k v
PatchDMapWithPatchingMove dstAfter srcAfter `mappendPatchDMapWithPatchingMoveSlow` PatchDMapWithPatchingMove dstBefore srcBefore = PatchDMapWithPatchingMove dst src
  where
    getDstAction k m = fromMaybe (From_Move k) $ DMap.lookup k m -- Any key that isn't present is treated as that key moving to itself
    removeRedundantDst toKey (From_Move fromKey) | isJust (toKey `geq` fromKey) = Nothing
    removeRedundantDst _ a = Just a
    f :: forall a. k a -> From k v a -> Maybe (From k v a)
    f toKey _ = removeRedundantDst toKey $ case getDstAction toKey dstAfter of
      From_Move fromKey -> getDstAction fromKey dstBefore
      nonMove -> nonMove
    dst = DMap.mapMaybeWithKey f $ DMap.union dstAfter dstBefore
    getSrcAction k m = fromMaybe (ComposeMaybe $ Just k) $ DMap.lookup k m
    removeRedundantSrc fromKey (ComposeMaybe (Just toKey)) | isJust (fromKey `geq` toKey) = Nothing
    removeRedundantSrc _ a = Just a
    g :: forall a. k a -> ComposeMaybe k a -> Maybe (ComposeMaybe k a)
    g fromKey _ = removeRedundantSrc fromKey $ case getSrcAction fromKey srcBefore of
      ComposeMaybe Nothing -> ComposeMaybe Nothing
      ComposeMaybe (Just toKeyBefore) -> getSrcAction toKeyBefore srcAfter
    src = DMap.mapMaybeWithKey g $ DMap.union srcAfter srcBefore
-}

-- | Make a @'PatchDMapWithPatchingMove' k v@ which has the effect of inserting or
-- updating a value @PatchTarget1 p a@ to the given key @k a@, like
-- 'DMap.insert'.
insertDMapKey :: k a -> PatchTarget1 p a -> PatchDMapWithPatchingMove k p
insertDMapKey k v =
  PatchDMapWithPatchingMove . DMap.singleton k $ NodeInfo (From_Insert v) To_NonMove

-- | Make a @'PatchDMapWithPatchingMove' k v@ which has the effect of moving the value
-- from the first key @k a@ to the second key @k a@, equivalent to:
--
-- @
--     'DMap.delete' src (maybe dmap ('DMap.insert' dst) (DMap.lookup src dmap))
-- @
moveDMapKey
  :: GCompare k
  => k a -> k a -> PatchDMapWithPatchingMove k (Proxy3 v)
moveDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithPatchingMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move (src :=> Flip Proxy3)) To_NonMove
    , src :=> NodeInfo From_Delete (To_Move $ Some dst)
    ]
  Just _ -> PatchDMapWithPatchingMove DMap.empty

-- | Make a @'PatchDMapWithPatchingMove' k v@ which has the effect of swapping two keys
-- in the mapping, equivalent to:
--
-- @
--     let aMay = DMap.lookup a dmap
--         bMay = DMap.lookup b dmap
--     in maybe id (DMap.insert a) (bMay <> aMay)
--      . maybe id (DMap.insert b) (aMay <> bMay)
--      . DMap.delete a . DMap.delete b $ dmap
-- @
swapDMapKey :: GCompare k => k a -> k a -> PatchDMapWithPatchingMove k (Proxy3 v)
swapDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithPatchingMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move (src :=> Flip Proxy3)) (To_Move $ Some src)
    , src :=> NodeInfo (From_Move (dst :=> Flip Proxy3)) (To_Move $ Some dst)
    ]
  Just _ -> PatchDMapWithPatchingMove DMap.empty

-- | Make a @'PatchDMapWithPatchingMove' k v@ which has the effect of deleting a key in
-- the mapping, equivalent to 'DMap.delete'.
deleteDMapKey :: k a -> PatchDMapWithPatchingMove k v
deleteDMapKey k = PatchDMapWithPatchingMove $ DMap.singleton k $ NodeInfo From_Delete To_NonMove

{-
k1, k2 :: Const2 Int () ()
k1 = Const2 1
k2 = Const2 2
p1, p2 :: PatchDMapWithPatchingMove (Const2 Int ()) Identity
p1 = moveDMapKey k1 k2
p2 = moveDMapKey k2 k1
p12 = p1 <> p2
p21 = p2 <> p1
p12Slow = p1 `mappendPatchDMapWithPatchingMoveSlow` p2
p21Slow = p2 `mappendPatchDMapWithPatchingMoveSlow` p1

testPatchDMapWithPatchingMove = do
  print p1
  print p2
  print $ p12 == deleteDMapKey k1
  print $ p21 == deleteDMapKey k2
  print $ p12Slow == deleteDMapKey k1
  print $ p21Slow == deleteDMapKey k2

dst (PatchDMapWithPatchingMove x _) = x
src (PatchDMapWithPatchingMove _ x) = x
-}

-- | Extract the 'DMap' representing the patch changes from the
-- 'PatchDMapWithPatchingMove'.
unPatchDMapWithPatchingMove :: PatchDMapWithPatchingMove k v -> DMap k (NodeInfo k v)
unPatchDMapWithPatchingMove (PatchDMapWithPatchingMove p) = p

-- | Wrap a 'DMap' representing patch changes into a 'PatchDMapWithPatchingMove',
-- without checking any invariants.
--
-- __Warning:__ when using this function, you must ensure that the invariants of 'PatchDMapWithPatchingMove' are preserved; they will not be checked.
unsafePatchDMapWithPatchingMove :: DMap k (NodeInfo k v) -> PatchDMapWithPatchingMove k v
unsafePatchDMapWithPatchingMove = PatchDMapWithPatchingMove

-- | Wrap a 'DMap' representing patch changes into a 'PatchDMapWithPatchingMove' while
-- checking invariants. If the invariants are satisfied, @Right p@ is returned
-- otherwise @Left errors@.
patchDMapWithPatchingMove :: (GCompare k, GShow k) => DMap k (NodeInfo k v) -> Either [String] (PatchDMapWithPatchingMove k v)
patchDMapWithPatchingMove dm =
  case validationErrorsForPatchDMapWithPatchingMove dm of
    [] -> Right $ unsafePatchDMapWithPatchingMove dm
    errs -> Left errs

-- | Map a natural transform @v -> v'@ over the given patch, transforming
-- @'PatchDMapWithPatchingMove' k v@ into @'PatchDMapWithPatchingMove' k v'@.
mapPatchDMapWithPatchingMove
  :: forall k p p'
  .  (forall a. PatchTarget1 p a -> PatchTarget1 p' a)
  -> (forall from to. p from to -> p' from to)
  -> PatchDMapWithPatchingMove k p
  -> PatchDMapWithPatchingMove k p'
mapPatchDMapWithPatchingMove f g (PatchDMapWithPatchingMove m) =
  PatchDMapWithPatchingMove $ DMap.map (\ni -> NodeInfo
    { _nodeInfo_from = h $ _nodeInfo_from ni
    , _nodeInfo_to = _nodeInfo_to ni
    }) m
  where h :: forall a. From k p a -> From k p' a
        h = \case
          From_Insert v -> From_Insert $ f v
          From_Delete -> From_Delete
          From_Move (k :=> Flip p) -> From_Move $ k :=> Flip (g p)

-- | Traverse an effectful function @forall a. PatchTarget1 p a -> m (v ' a)@
-- over the given patch, transforming @'PatchDMapWithPatchingMove' k v@ into @m
-- ('PatchDMapWithPatchingMove' k v')@.
traversePatchDMapWithPatchingMove
  :: forall m k p p'
  . Applicative m
  => (forall a. PatchTarget1 p a -> m (PatchTarget1 p' a))
  -> (forall from to. p from to -> m (p' from to))
  -> PatchDMapWithPatchingMove k p
  -> m (PatchDMapWithPatchingMove k p')
traversePatchDMapWithPatchingMove f g = traversePatchDMapWithPatchingMoveWithKey
  (\_ -> f)
  (\_ _ -> g)

-- | Map an effectful function @forall a. k a -> PatchTarget1 p a -> m (v ' a)@
-- over the given patch, transforming @'PatchDMapWithPatchingMove' k v@ into @m
-- ('PatchDMapWithPatchingMove' k v')@.
traversePatchDMapWithPatchingMoveWithKey
  :: forall m k p p'
  . Applicative m
  => (forall a. k a -> PatchTarget1 p a -> m (PatchTarget1 p' a))
  -> (forall from to. k from -> k to -> p from to -> m (p' from to))
  -> PatchDMapWithPatchingMove k p
  -> m (PatchDMapWithPatchingMove k p')
traversePatchDMapWithPatchingMoveWithKey f g (PatchDMapWithPatchingMove m) =
  PatchDMapWithPatchingMove <$> DMap.traverseWithKey (\k ni -> NodeInfo
    <$> h k (_nodeInfo_from ni)
    <*> pure (_nodeInfo_to ni)) m
  where h :: forall a. k a -> From k p a -> m (From k p' a)
        h k = \case
          From_Insert v -> From_Insert <$> f k v
          From_Delete -> pure From_Delete
          From_Move (fromKey :=> Flip p) -> From_Move . (fromKey :=>) . Flip <$> g fromKey k p

-- | Map a function which transforms @'From k PatchTarget1 p a@ into a @'From k
-- PatchTarget1 p' a@ over a @'NodeInfo' k PatchTarget1 p a@.
nodeInfoMapFrom
  :: (From k p a -> From k p' a)
  -> (To k -> To k)
  -> NodeInfo k p a
  -> NodeInfo k p' a
nodeInfoMapFrom f g ni = NodeInfo
  { _nodeInfo_from = f $ _nodeInfo_from ni
  , _nodeInfo_to = g $ _nodeInfo_to ni
  }

-- | Map an effectful function which transforms @'From k PatchTarget1 p a@ into
-- a @f ('From k PatchTarget1 p' a)@ over a @'NodeInfo' k PatchTarget1 p a@.
nodeInfoMapFromM
  :: Applicative f
  => (From k p a -> f (From k p' a))
  -> (To k -> f (To k))
  -> NodeInfo k p a
  -> f (NodeInfo k p' a)
nodeInfoMapFromM f g ni = NodeInfo
  <$> f (_nodeInfo_from ni)
  <*> g (_nodeInfo_to ni)

-- | Weaken a 'PatchDMapWithPatchingMove' to a 'PatchMapWithPatchingMove' by weakening the keys
-- from @k a@ to @'Some' k@ and applying a given weakening function
-- @PatchTarget1 p a -> v'@ to values.
weakenPatchDMapWithPatchingMoveWith
  :: forall k p p'
  .  (forall a. PatchTarget1 p a -> PatchTarget p')
  -> (forall from to. p from to -> p')
  -> PatchDMapWithPatchingMove k p
  -> PatchMapWithPatchingMove (Some k) p'
weakenPatchDMapWithPatchingMoveWith f g (PatchDMapWithPatchingMove m) =
  PatchMapWithPatchingMove $ weakenDMapWith h m
  where h :: forall a. NodeInfo k p a -> MapWithPatchingMove.NodeInfo (Some k) p'
        h ni = MapWithPatchingMove.NodeInfo
          { MapWithPatchingMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithPatchingMove.From_Insert $ f v
              From_Delete -> MapWithPatchingMove.From_Delete
              From_Move (k :=> Flip p) -> MapWithPatchingMove.From_Move (mkSome k) $ g p
          , MapWithPatchingMove._nodeInfo_to = case _nodeInfo_to ni of
              To_NonMove -> Nothing
              To_Move (Some k) -> Just (mkSome k)
          }

-- | "Weaken" a @'PatchDMapWithPatchingMove' (Const2 k a) v@ to a @'PatchMapWithPatchingMove' k
-- v'@. Weaken is in scare quotes because the 'Const2' has already disabled any
-- dependency in the typing and all points are already @a@, hence the function
-- to map each value to @v'@ is not higher rank.
patchDMapWithPatchingMoveToPatchMapWithPatchingMoveWith
  :: forall k p p' a
  . (PatchTarget1 p a -> PatchTarget p')
  -> (p a a -> p')
  -> PatchDMapWithPatchingMove (Const2 k a) p
  -> PatchMapWithPatchingMove k p'
patchDMapWithPatchingMoveToPatchMapWithPatchingMoveWith f g (PatchDMapWithPatchingMove m) =
  PatchMapWithPatchingMove $ dmapToMapWith h m
  where h :: NodeInfo (Const2 k a) p a -> MapWithPatchingMove.NodeInfo k p'
        h ni = MapWithPatchingMove.NodeInfo
          { MapWithPatchingMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithPatchingMove.From_Insert $ f v
              From_Delete -> MapWithPatchingMove.From_Delete
              From_Move (Const2 k :=> Flip p) -> MapWithPatchingMove.From_Move k $ g p
          , MapWithPatchingMove._nodeInfo_to = case _nodeInfo_to ni of
              To_NonMove -> Nothing
              To_Move (Some (Const2 k)) -> Just k
          }

-- | "Strengthen" a @'PatchMapWithPatchingMove' k v@ into a @'PatchDMapWithPatchingMove
-- ('Const2' k a)@; that is, turn a non-dependently-typed patch into a
-- dependently typed one but which always has a constant key type represented by
-- 'Const2'. Apply the given function to each @v@ to produce a @PatchTarget1 p'
-- a@. Completemented by 'patchDMapWithPatchingMoveToPatchMapWithPatchingMoveWith'
const2PatchDMapWithPatchingMoveWith
  :: forall k v v' a
  .  (v -> v' a)
  -> PatchMapWithPatchingMove k (Proxy v)
  -> PatchDMapWithPatchingMove (Const2 k a) (Proxy3 v')
const2PatchDMapWithPatchingMoveWith f (PatchMapWithPatchingMove p) =
  PatchDMapWithPatchingMove $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, MapWithPatchingMove.NodeInfo k (Proxy v))
          -> DSum (Const2 k a) (NodeInfo (Const2 k a) (Proxy3 v'))
        g (k, ni) = Const2 k :=> NodeInfo
          { _nodeInfo_from = case MapWithPatchingMove._nodeInfo_from ni of
              MapWithPatchingMove.From_Insert v -> From_Insert $ f v
              MapWithPatchingMove.From_Delete -> From_Delete
              MapWithPatchingMove.From_Move fromKey Proxy -> From_Move $ Const2 fromKey :=> Flip Proxy3
          , _nodeInfo_to = case MapWithPatchingMove._nodeInfo_to ni of
              Nothing -> To_NonMove
              Just toKey -> To_Move $ Some (Const2 toKey)
          }

-- | Apply the insertions, deletions, and moves to a given 'DMap'.
instance ( GCompare k
         , PatchHet2 p
         , PatchSource1 p ~ PatchTarget1 p
         ) => PatchHet (PatchDMapWithPatchingMove k p) where
  type PatchSource (PatchDMapWithPatchingMove k p) = DMap k (PatchSource1 p)
  type PatchTarget (PatchDMapWithPatchingMove k p) = DMap k (PatchTarget1 p)

instance ( GCompare k
         , PatchHet2 p
         , PatchSource1 p ~ PatchTarget1 p
         ) => Patch (PatchDMapWithPatchingMove k p) where
  apply (PatchDMapWithPatchingMove m) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions)
    -- TODO: return Nothing sometimes --Note: the strict application here is
    -- critical to ensuring that incremental merges don't hold onto all their
    -- prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey insertFunc m
          insertFunc :: forall a. k a -> NodeInfo k p a -> Maybe (PatchTarget1 p a)
          insertFunc _ ni = case _nodeInfo_from ni of
            From_Insert v -> Just v
            From_Move (k :=> Flip p) -> applyAlwaysHet2 p <$> DMap.lookup k old
            From_Delete -> Nothing
          deletions = DMap.mapMaybeWithKey deleteFunc m
          deleteFunc :: forall a. k a -> NodeInfo k p a -> Maybe (Constant () a)
          deleteFunc _ ni = case _nodeInfo_from ni of
            From_Delete -> Just $ Constant ()
            _ -> Nothing

-- | Get the values that will be replaced, deleted, or moved if the given patch
-- is applied to the given 'DMap'.
getDeletionsAndMoves
  :: ( GCompare k
     , PatchSource1 p ~ PatchTarget1 p
     )
  => PatchDMapWithPatchingMove k p
  -> DMap k (PatchSource1 p)
  -> DMap k (Product (PatchTarget1 p) (Const (To k)))
getDeletionsAndMoves (PatchDMapWithPatchingMove p) m = DMap.intersectionWithKey f m p
  where f _ v ni = Pair v $ Const $ _nodeInfo_to ni
