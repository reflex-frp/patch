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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |Module containing @'PatchDMapWithMove' k v@ and associated functions, which represents a 'Patch' to a @'DMap' k v@ which can insert, update, delete, and
-- move values between keys.
module Data.Patch.DMapWithMove where

import qualified Control.Category as Cat
import Data.Constraint.Extras
import Data.Constraint.Compose
import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare (GEq (..), GCompare)
import Data.GADT.Show (GRead, GShow, gshow)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid.DecidablyEmpty
import Data.Semigroup (Semigroup (..), (<>))
import Data.Semigroupoid as Cat
import Data.Some (Some(Some))
import Data.Proxy
import Data.These
import Data.Type.Equality ((:~:) (..))
import Data.Kind (Constraint)

import Data.Patch.Class
import Data.Patch.MapWithMove (PatchMapWithMove (..))
import qualified Data.Patch.MapWithMove as MapWithMove

-- | Composition for constraints.
class p (f a a) => DupC (p :: k2 -> Constraint) (f :: k1 -> k1 -> k2) (a :: k1)
instance p (f a a) => DupC p f a

type ConstraintsForZip f (c :: k -> Constraint) (g :: k' -> k' -> k) =
  ConstraintsFor f (DupC c g)

type HasZip (c :: k -> Constraint) f (g :: k' -> k' -> k) =
  (ArgDict (DupC c g) f, ConstraintsForZip f c g)

-- | Like 'PatchMapWithMove', but for 'DMap'. Each key carries a 'NodeInfo'
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
newtype PatchDMapWithMove k v = PatchDMapWithMove (DMap k (NodeInfo k v))

deriving instance ( GShow k
                  , HasZip Show k p
                  , Has' Show k (PatchTarget1 p)
                  ) => Show (PatchDMapWithMove k p)
deriving instance ( GRead k
                  , HasZip Read k p
                  , Has' Read k (PatchTarget1 p)
                  ) => Read (PatchDMapWithMove k p)
deriving instance ( GEq k
                  , HasZip Eq k p
                  , Has' Eq k (PatchTarget1 p)
                  ) => Eq (PatchDMapWithMove k p)
deriving instance ( GCompare k
                  , HasZip Ord k p
                  , Has' Ord k (PatchTarget1 p)
                  ) => Ord (PatchDMapWithMove k p)

-- | Structure which represents what changes apply to a particular key.
-- @_nodeInfo_from@ specifies what happens to this key, and in particular what
-- other key the current key is moving from, while @_nodeInfo_to@ specifies what
-- key the current key is moving to if involved in a move.
data NodeInfo k p a = NodeInfo
  { _nodeInfo_from :: !(From k p a)
  -- ^ Change applying to the current key, be it an insert, move, or delete.
  , _nodeInfo_to :: !(To k p a)
  -- ^ Where this key is moving to, if involved in a move. Should only be
  -- @ComposeMaybe (Just k)@ when there is a corresponding 'From_Move'.
  }

deriving instance ( Show (k a)
                  , Show (p a a)
                  , Show (PatchTarget1 p a)
                  ) => Show (NodeInfo k p a)
deriving instance ( Read (k a)
                  , Read (p a a)
                  , Read (PatchTarget1 p a)
                  ) => Read (NodeInfo k p a)
deriving instance ( Eq (k a)
                  , Eq (p a a)
                  , Eq (PatchTarget1 p a)
                  ) => Eq (NodeInfo k p a)
deriving instance ( Ord (k a)
                  , Ord (p a a)
                  , Ord (PatchTarget1 p a)
                  ) => Ord (NodeInfo k p a)

-- | Structure describing a particular change to a key, be it inserting a new
-- key (@By_Insert@), updating an existing key (@By_Insert@ again), deleting
-- a key (@By_Delete@), or moving a key (@By_Move@).
--
-- This type isn't used directly as the from field patch, but is instead wrapped
-- in an existential. However, it is nice to be able to reason about this in
-- isolation as it is itself a @Semigroupoid@ when the underlying patch is.
data By (k :: a -> *) (p :: a -> a -> *) :: a -> a -> * where
  -- | Insert a new or update an existing key with the given value @PatchTarget1
  -- p a@
  By_Insert :: PatchTarget1 p to -> By k p from to
  -- | Delete the existing key
  By_Delete :: By k p from to
  -- | Move the value from the given key @k a@ to this key. The source key
  -- should also have an entry in the patch giving the current key as
  -- @_nodeInfo_to@, usually but not necessarily with @By_Delete@.
  By_Move :: !(k from) -> p from to -> By k p from to

deriving instance ( Show (k to)
                  , Show (p from to)
                  , Show (PatchTarget1 p to)
                  ) => Show (By k p from to)
deriving instance ( Read (k to)
                  , Read (p from to)
                  , Read (PatchTarget1 p to)
                  ) => Read (By k p from to)
deriving instance ( Eq (k to)
                  , Eq (p from to)
                  , Eq (PatchTarget1 p to)
                  ) => Eq (By k p from to)
deriving instance ( Ord (k to)
                  , Ord (p from to)
                  , Ord (PatchTarget1 p to)
                  ) => Ord (By k p from to)

mapByPatch
  :: PatchTarget1 p0 ~ PatchTarget1 p1
  => ((p0 from to) -> (p1 from to))
  -> By k p0 from to
  -> By k p1 from to
mapByPatch f = \case
  By_Insert v -> By_Insert v
  By_Delete -> By_Delete
  By_Move k p -> By_Move k $ f p

-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance ( PatchSource1 p ~ PatchTarget1 p
         , Cat.Semigroupoid p
         , PatchHet2 p
         ) => Cat.Semigroupoid (By k p) where
  o p0 p1 = mapByPatch unProjectLocal $
    oLocal (mapByPatch ProjectLocal p0) (mapByPatch ProjectLocal p1)

oLocal
  :: ( PatchSource1 p ~ PatchTarget1 p
     , PatchHet2Locally p between after
     )
  => By k p between after
  -> By k p before between
  -> By k p before after
By_Insert new `oLocal` _ = By_Insert new
By_Delete `oLocal` _ = By_Delete
By_Move _ x `oLocal` By_Insert y = By_Insert $ applyAlwaysHet2 x y
By_Move _ x `oLocal` By_Move src y = By_Move src $ x `o` y
By_Move _ _ `oLocal` By_Delete = By_Delete

newtype Flip p to from = Flip (p from to)

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

deriving instance ( Show (k a)
                  , Show (p a a)
                  , Show (PatchTarget1 p a)
                  ) => Show (From k p a)
deriving instance ( Read (k a)
                  , Read (p a a)
                  , Read (PatchTarget1 p a)
                  ) => Read (From k p a)

deriving instance ( GEq k
                  , HasZip Eq k p
                  , Eq (PatchTarget1 p a)
                  ) => Eq (From k p a)

deriving instance ( GCompare k
                  , HasZip Ord k p
                  , Ord (PatchTarget1 p a)
                  ) => Ord (From k p a)

-- | The "to" part of a 'NodeInfo'. Rather than be built out of @From@ like @From@
-- is, we store just the information necessary to compose a @To@ and @From@ like
-- @oLocal@ composes two @From@s.
data To (k :: a -> *) (p :: a -> a -> *) :: a -> * where
  -- | Delete or leave in place
  To_NonMove :: To k p a0
  -- | Move the value from the given key @k a@ to this key. The target key
  -- should also have an entry in the patch giving the current key in
  -- @_nodeInfo_from@, usually but not necessarily with @To_Delete@.
  To_Move :: !(k a1) -> p a0 a1 -> To k p a0

deriving instance ( Show (k a)
                  , Show (p a a)
                  , Show (PatchTarget1 p a)
                  ) => Show (To k p a)
deriving instance ( Read (k a)
                  , Read (p a a)
                  , Read (PatchTarget1 p a)
                  ) => Read (To k p a)

instance ( GEq k
         , HasZip Eq k p
         , Eq (PatchTarget1 p a)
         ) => Eq (To k p a) where
  To_NonMove == To_NonMove = True
  To_Move k0 p0 == To_Move k1 p1 = case geq k0 k1 of
    Nothing -> False
    Just Refl -> has @(DupC Eq p) k0 $ p0 == p1

instance ( GCompare k
         , HasZip Ord k p
         , Ord (PatchTarget1 p a)
         ) => Ord (To k p a) where
  To_NonMove `compare` To_NonMove = EQ
  To_Move k0 p0 `compare` To_Move k1 p1 = case gcompare k0 k1 of
    DMap.GGT -> GT
    DMap.GEQ -> has @(DupC Ord p) k0 $ p0 `compare` p1
    DMap.GLT -> LT

--oToFrom
--  :: ( PatchSource1 p ~ PatchTarget1 p
--     , PatchHet2 p
--     )
--  => To k p between
--  -> From k p between
--  -> DSum k (From k p)
--From_Insert new `oToFrom` _ = From_Insert new
--From_Delete `oToFrom` _ = From_Delete
--From_Move _ x `oToFrom` From_Insert y = From_Insert $ applyAlwaysHet2 x y
--From_Move _ x `oToFrom` From_Move src y = From_Move src $ x `o` y
--From_Move _ _ `oToFrom` From_Delete = From_Delete

-- |Test whether a 'PatchDMapWithMove' satisfies its invariants.
validPatchDMapWithMove
  :: forall k v
  .  (GCompare k, GShow k)
  => DMap k (NodeInfo k v)
  -> Bool
validPatchDMapWithMove = not . null . validationErrorsForPatchDMapWithMove

-- |Enumerate what reasons a 'PatchDMapWithMove' doesn't satisfy its invariants, returning @[]@ if it's valid.
validationErrorsForPatchDMapWithMove
  :: forall k v
  .  (GCompare k, GShow k)
  => DMap k (NodeInfo k v)
  -> [String]
validationErrorsForPatchDMapWithMove m =
  noSelfMoves <> movesBalanced
  where
    noSelfMoves = mapMaybe selfMove . DMap.toAscList $ m
    selfMove (dst :=> NodeInfo (From_Move (src :=> _)) _)
      | Just _ <- dst `geq` src = Just $ "self move of key " <> gshow src <> " at destination side"
    selfMove (src :=> NodeInfo _ (To_Move dst _))
      | Just _ <- src `geq` dst = Just $ "self move of key " <> gshow dst <> " at source side"
    selfMove _ = Nothing
    movesBalanced = mapMaybe unbalancedMove . DMap.toAscList $ m
    unbalancedMove (dst :=> NodeInfo (From_Move (src :=> _)) _) =
      case DMap.lookup src m of
        Nothing -> Just $ "unbalanced move at destination key " <> gshow dst <> " supposedly from " <> gshow src <> " but source key is not in the patch"
        Just (NodeInfo _ (To_Move dst' _)) ->
          if isNothing (dst' `geq` dst)
            then Just $ "unbalanced move at destination key " <> gshow dst <> " from " <> gshow src <> " is going to " <> gshow dst' <> " instead"
            else Nothing
        _ ->
          Just $ "unbalanced move at destination key " <> gshow dst <> " supposedly from " <> gshow src <> " but source key has no move to key"
    unbalancedMove (src :=> NodeInfo _ (To_Move dst _)) =
      case DMap.lookup dst m of
        Nothing -> Just $ " unbalanced move at source key " <> gshow src <> " supposedly going to " <> gshow dst <> " but destination key is not in the patch"
        Just (NodeInfo (From_Move (src' :=> _)) _) ->
          if isNothing (src' `geq` src)
            then Just $ "unbalanced move at source key " <> gshow src <> " to " <> gshow dst <> " is coming from " <> gshow src' <> " instead"
            else Nothing

        _ ->
          Just $ "unbalanced move at source key " <> gshow src <> " supposedly going to " <> gshow dst <> " but destination key is not moving"
    unbalancedMove _ = Nothing

-- |Higher kinded 2-tuple, identical to @Data.Functor.Product@ from base ≥ 4.9
data Pair1 f g a = Pair1 (f a) (g a)

-- |Helper data structure used for composing patches using the monoid instance.
data Fixup k p a
   = Fixup_Delete
   | Fixup_Update (These (From k p a) (To k p a))

-- | Compose patches having the same effect as applying the patches in turn:
-- @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance ( GCompare k
         , Cat.Semigroupoid p
         -- , DecidablyEmpty p
         , PatchHet2 p
         ) => Semigroup (PatchDMapWithMove k p) where
  PatchDMapWithMove ma <> PatchDMapWithMove mb = PatchDMapWithMove m
    where
      connections :: [DSum k (Pair1 (To k p) (From k p))]
      connections = DMap.toList $ DMap.intersectionWithKey
        (\_ a b -> Pair1 (_nodeInfo_to a) (_nodeInfo_from b))
        ma
        mb
      h :: DSum k (Pair1 k (From k v)) -> [DSum k (Fixup k v)]
      h (between :=> Pair1 editAfter editBefore) = case (editAfter, editBefore) of
        (To_Move toAfter p1, From_Move (fromBefore :=> p0)) -> case toAfter `geq` fromBefore of
          Just Refl | isNull p0 ->
            [ toAfter :=> Fixup_Delete ]
          _ ->
            [ toAfter :=> Fixup_Update (This editBefore)
            , fromBefore :=> Fixup_Update (That editAfter)
            ]
        (To_NonMove, From_Move (fromBefore :=> _)) ->
          -- The item is destroyed in the second patch, so indicate that it is
          -- destroyed in the source map
          [fromBefore :=> Fixup_Update (That (ComposeMaybe editAfter))]
        (Just toAfter, _) -> [toAfter :=> Fixup_Update (This editBefore)]
        (Nothing, _) -> []
      mergeFixups _ Fixup_Delete Fixup_Delete = Fixup_Delete
      mergeFixups _ (Fixup_Update a) (Fixup_Update b)
        | This x <- a, That y <- b
        = Fixup_Update $ These x y
        | That y <- a, This x <- b
        = Fixup_Update $ These x y
      mergeFixups _ _ _ = error "PatchDMapWithMove: incompatible fixups"
      fixups = DMap.fromListWithKey mergeFixups $ concatMap h connections
      combineNodeInfos _ nia nib = NodeInfo
        { _nodeInfo_from = _nodeInfo_from nia
        , _nodeInfo_to = _nodeInfo_to nib
        }
      applyFixup _ ni = \case
        Fixup_Delete -> Nothing
        Fixup_Update u -> Just $ NodeInfo
          { _nodeInfo_from = case _nodeInfo_from ni of
              f@(From_Move (_ :=> p')) -> case _here u of -- The `from` fixup comes from the "old" patch
                Nothing -> f -- If there's no `from` fixup, just use the "new" `from`
                Just (From_Insert v) -> From_Insert $ applyAlways p' v
                Just From_Delete -> From_Delete
                Just (From_Move (oldKey :=> p)) -> From_Move $ oldKey :=> (p' <> p)
              _ -> error "DPatchMapWithMove: fixup for non-move From"
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

-- |Compose patches having the same effect as applying the patches in turn: @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance GCompare k => Monoid (PatchDMapWithMove k v) where
  mempty = PatchDMapWithMove mempty
  mappend = (<>)

{-
mappendPatchDMapWithMoveSlow :: forall k v.  (ShowTag k v, GCompare k) => PatchDMapWithMove k v -> PatchDMapWithMove k v -> PatchDMapWithMove k v
PatchDMapWithMove dstAfter srcAfter `mappendPatchDMapWithMoveSlow` PatchDMapWithMove dstBefore srcBefore = PatchDMapWithMove dst src
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

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of inserting or updating a value @PatchTarget1 p a@ to the given key @k a@, like 'DMap.insert'.
insertDMapKey :: k a -> PatchTarget1 p a -> PatchDMapWithMove k p
insertDMapKey k v =
  PatchDMapWithMove . DMap.singleton k $ NodeInfo (From_Insert v) (ComposeMaybe Nothing)

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of moving the value from the first key @k a@ to the second key @k a@, equivalent to:
--
-- @
--     'DMap.delete' src (maybe dmap ('DMap.insert' dst) (DMap.lookup src dmap))
-- @
moveDMapKey :: GCompare k => k a -> k a -> PatchDMapWithMove k v
moveDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move (src :=> Cat.id)) (ComposeMaybe Nothing)
    , src :=> NodeInfo From_Delete (ComposeMaybe $ Just dst)
    ]
  Just _ -> mempty

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of swapping two keys in the mapping, equivalent to:
--
-- @
--     let aMay = DMap.lookup a dmap
--         bMay = DMap.lookup b dmap
--     in maybe id (DMap.insert a) (bMay `mplus` aMay)
--      . maybe id (DMap.insert b) (aMay `mplus` bMay)
--      . DMap.delete a . DMap.delete b $ dmap
-- @
swapDMapKey :: GCompare k => k a -> k a -> PatchDMapWithMove k v
swapDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move (src :=> Cat.id)) (ComposeMaybe $ Just src)
    , src :=> NodeInfo (From_Move (dst :=> Cat.id)) (ComposeMaybe $ Just dst)
    ]
  Just _ -> mempty

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of deleting a key in the mapping, equivalent to 'DMap.delete'.
deleteDMapKey :: k a -> PatchDMapWithMove k v
deleteDMapKey k = PatchDMapWithMove $ DMap.singleton k $ NodeInfo From_Delete $ ComposeMaybe Nothing

{-
k1, k2 :: Const2 Int () ()
k1 = Const2 1
k2 = Const2 2
p1, p2 :: PatchDMapWithMove (Const2 Int ()) Identity
p1 = moveDMapKey k1 k2
p2 = moveDMapKey k2 k1
p12 = p1 <> p2
p21 = p2 <> p1
p12Slow = p1 `mappendPatchDMapWithMoveSlow` p2
p21Slow = p2 `mappendPatchDMapWithMoveSlow` p1

testPatchDMapWithMove = do
  print p1
  print p2
  print $ p12 == deleteDMapKey k1
  print $ p21 == deleteDMapKey k2
  print $ p12Slow == deleteDMapKey k1
  print $ p21Slow == deleteDMapKey k2

dst (PatchDMapWithMove x _) = x
src (PatchDMapWithMove _ x) = x
-}

-- |Extract the 'DMap' representing the patch changes from the 'PatchDMapWithMove'.
unPatchDMapWithMove :: PatchDMapWithMove k v -> DMap k (NodeInfo k v)
unPatchDMapWithMove (PatchDMapWithMove p) = p

-- |Wrap a 'DMap' representing patch changes into a 'PatchDMapWithMove', without checking any invariants.
--
-- __Warning:__ when using this function, you must ensure that the invariants of 'PatchDMapWithMove' are preserved; they will not be checked.
unsafePatchDMapWithMove :: DMap k (NodeInfo k v) -> PatchDMapWithMove k v
unsafePatchDMapWithMove = PatchDMapWithMove

-- |Wrap a 'DMap' representing patch changes into a 'PatchDMapWithMove' while checking invariants. If the invariants are satisfied, @Right p@ is returned
-- otherwise @Left errors@.
patchDMapWithMove :: (GCompare k, GShow k) => DMap k (NodeInfo k v) -> Either [String] (PatchDMapWithMove k v)
patchDMapWithMove dm =
  case validationErrorsForPatchDMapWithMove dm of
    [] -> Right $ unsafePatchDMapWithMove dm
    errs -> Left errs

-- |Map a natural transform @v -> v'@ over the given patch, transforming @'PatchDMapWithMove' k v@ into @'PatchDMapWithMove' k v'@.
mapPatchDMapWithMove
  :: forall k p p'
  .  (forall a. PatchTarget1 p a -> PatchTarget1 p' a)
  -> (forall from to. p from to -> p' from to)
  -> PatchDMapWithMove k p
  -> PatchDMapWithMove k p'
mapPatchDMapWithMove f g (PatchDMapWithMove p) = PatchDMapWithMove $
  DMap.map (\ni -> ni { _nodeInfo_from = g $ _nodeInfo_from ni }) p
  where g :: forall a. From k PatchTarget1 p a -> From k PatchTarget1 p' a
        g = \case
          From_Insert v -> From_Insert $ f v
          From_Delete -> From_Delete
          From_Move k p -> From_Move k $ g p

-- | Traverse an effectful function @forall a. PatchTarget1 p a -> m (v ' a)@
-- over the given patch, transforming @'PatchDMapWithMove' k v@ into @m
-- ('PatchDMapWithMove' k v')@.
traversePatchDMapWithMove
  :: forall m k p p'
  . Applicative m
  => (forall a. PatchTarget1 p a -> m (PatchTarget1 p' a))
  -> PatchDMapWithMove k p
  -> m (PatchDMapWithMove k p')
traversePatchDMapWithMove f = traversePatchDMapWithMoveWithKey $ const f

-- | Map an effectful function @forall a. k a -> PatchTarget1 p a -> m (v ' a)@
-- over the given patch, transforming @'PatchDMapWithMove' k v@ into @m
-- ('PatchDMapWithMove' k v')@.
traversePatchDMapWithMoveWithKey
  :: forall m k p p'
  . Applicative m
  => (forall a. k a -> PatchTarget1 p a -> m (PatchTarget1 p' a))
  -> PatchDMapWithMove k p
  -> m (PatchDMapWithMove k p')
traversePatchDMapWithMoveWithKey f (PatchDMapWithMove p) =
  PatchDMapWithMove <$> DMap.traverseWithKey (nodeInfoMapFromM . g) p
  where g :: forall a. k a -> From k PatchTarget1 p a -> m (From k PatchTarget1 p' a)
        g k = \case
          From_Insert v -> From_Insert <$> f k v
          From_Delete -> pure From_Delete
          From_Move fromKey -> pure $ From_Move fromKey

-- | Map a function which transforms @'From k PatchTarget1 p a@ into a @'From k
-- PatchTarget1 p' a@ over a @'NodeInfo' k PatchTarget1 p a@.
nodeInfoMapFrom
  :: (From k p a -> From k p' a)
  -> NodeInfo k p a
  -> NodeInfo k p' a
nodeInfoMapFrom f ni = ni { _nodeInfo_from = f $ _nodeInfo_from ni }

-- | Map an effectful function which transforms @'From k PatchTarget1 p a@ into
-- a @f ('From k PatchTarget1 p' a)@ over a @'NodeInfo' k PatchTarget1 p a@.
nodeInfoMapFromM
  :: Functor f
  => (From k p a -> f (From k p' a))
  -> NodeInfo k p a
  -> f (NodeInfo k p' a)
nodeInfoMapFromM f ni = fmap (\result -> ni { _nodeInfo_from = result }) $ f $ _nodeInfo_from ni

-- | Weaken a 'PatchDMapWithMove' to a 'PatchMapWithMove' by weakening the keys
-- from @k a@ to @'Some' k@ and applying a given weakening function
-- @PatchTarget1 p a -> v'@ to values.
weakenPatchDMapWithMoveWith
  :: forall k p v'
  .  (forall a. PatchTarget1 p a -> v')
  -> PatchDMapWithMove k p
  -> PatchMapWithMove (Some k) (Proxy v')
weakenPatchDMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ weakenDMapWith g p
  where g :: forall a. NodeInfo k PatchTarget1 p a -> MapWithMove.NodeInfo (Some k) (Proxy v')
        g ni = MapWithMove.NodeInfo
          { MapWithMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithMove.From_Insert $ f v
              From_Delete -> MapWithMove.From_Delete
              From_Move k -> MapWithMove.From_Move (Some k) Proxy
          , MapWithMove._nodeInfo_to = Some <$> getComposeMaybe (_nodeInfo_to ni)
          }

-- | "Weaken" a @'PatchDMapWithMove' (Const2 k a) v@ to a @'PatchMapWithMove' k
-- v'@. Weaken is in scare quotes because the 'Const2' has already disabled any
-- dependency in the typing and all points are already @a@, hence the function
-- to map each value to @v'@ is not higher rank.
patchDMapWithMoveToPatchMapWithMoveWith
  :: forall k p v' a. (PatchTarget1 p a -> v')
  -> PatchDMapWithMove (Const2 k a) p
  -> PatchMapWithMove k (Proxy v')
patchDMapWithMoveToPatchMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ dmapToMapWith g p
  where g :: NodeInfo (Const2 k a) PatchTarget1 p a -> MapWithMove.NodeInfo k (Proxy v')
        g ni = MapWithMove.NodeInfo
          { MapWithMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithMove.From_Insert $ f v
              From_Delete -> MapWithMove.From_Delete
              From_Move (Const2 k) -> MapWithMove.From_Move k Proxy
          , MapWithMove._nodeInfo_to = unConst2 <$> getComposeMaybe (_nodeInfo_to ni)
          }

-- | "Strengthen" a @'PatchMapWithMove' k v@ into a @'PatchDMapWithMove
-- ('Const2' k a)@; that is, turn a non-dependently-typed patch into a
-- dependently typed one but which always has a constant key type represented by
-- 'Const2'. Apply the given function to each @v@ to produce a @PatchTarget1 p'
-- a@. Completemented by 'patchDMapWithMoveToPatchMapWithMoveWith'
const2PatchDMapWithMoveWith
  :: forall k v p' a
  .  (v -> PatchTarget1 p' a)
  -> PatchMapWithMove k (Proxy v)
  -> PatchDMapWithMove (Const2 k a) p'
const2PatchDMapWithMoveWith f (PatchMapWithMove p) = PatchDMapWithMove $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, MapWithMove.NodeInfo k (Proxy v)) -> DSum (Const2 k a) (NodeInfo (Const2 k a) v')
        g (k, ni) = Const2 k :=> NodeInfo
          { _nodeInfo_from = case MapWithMove._nodeInfo_from ni of
              MapWithMove.From_Insert v -> From_Insert $ f v
              MapWithMove.From_Delete -> From_Delete
              MapWithMove.From_Move fromKey Proxy -> From_Move $ Const2 fromKey
          , _nodeInfo_to = ComposeMaybe $ Const2 <$> MapWithMove._nodeInfo_to ni
          }

-- | Apply the insertions, deletions, and moves to a given 'DMap'.
instance GCompare k => PatchHet (PatchDMapWithMove k p) where
  type PatchSource (PatchDMapWithMove k p) = DMap k (PatchSource1 p)
  type PatchTarget (PatchDMapWithMove k p) = DMap k (PatchTarget1 p)
instance GCompare k => Patch (PatchDMapWithMove k p) where
  apply (PatchDMapWithMove p) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions)
    -- TODO: return Nothing sometimes --Note: the strict application here is
    -- critical to ensuring that incremental merges don't hold onto all their
    -- prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey insertFunc p
          insertFunc :: forall a. k a -> NodeInfo k p a -> Maybe (PatchTarget1 p a)
          insertFunc _ ni = case _nodeInfo_from ni of
            From_Insert v -> Just v
            From_Move k -> DMap.lookup k old
            From_Delete -> Nothing
          deletions = DMap.mapMaybeWithKey deleteFunc p
          deleteFunc :: forall a. k a -> NodeInfo k p a -> Maybe (Constant () a)
          deleteFunc _ ni = case _nodeInfo_from ni of
            From_Delete -> Just $ Constant ()
            _ -> Nothing

-- | Get the values that will be replaced, deleted, or moved if the given patch
-- is applied to the given 'DMap'.
getDeletionsAndMoves :: GCompare k => PatchDMapWithMove k v -> DMap k v' -> DMap k (Product v' (ComposeMaybe k))
getDeletionsAndMoves (PatchDMapWithMove p) m = DMap.intersectionWithKey f m p
  where f _ v ni = Pair v $ _nodeInfo_to ni
