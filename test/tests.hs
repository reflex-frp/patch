{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.HUnit (runTestTT, (~:), assertEqual, errors, failures, test)
import Data.Patch ( Patch(apply) )
import Data.Patch.MapWithMove ( patchThatChangesMap )
import Data.Patch.MapWithPatchingMove (PatchMapWithPatchingMove)
import qualified Data.Patch.MapWithPatchingMove as PatchMapWithPatchingMove
import Data.Map as Map ( Map, fromList, singleton )
import Hedgehog (checkParallel, discover, Property, property, forAll, PropertyT, (===))
import Hedgehog.Gen as Gen ( int )
import Hedgehog.Range as Range ( linear )
import Control.Monad (replicateM)
import System.Exit (exitFailure, exitSuccess)
import Data.Sequence as Seq ( foldMapWithIndex, replicateM )
import Data.Semigroup
  ( Sum (..)
#if !MIN_VERSION_base(4,11,0)
  , Semigroup(..)
#endif
  )

main :: IO ()
main = do
   counts <- runTestTT $ test
     [ "Simple Move" ~: do
         let mapBefore = Map.fromList [(0,1)]
             mapAfter = Map.fromList [(0,0),(1,1)]
             patch = patchThatChangesMap mapBefore mapAfter
             afterPatch = apply patch mapBefore
         assertEqual "Patch creates the same Map" (Just mapAfter) afterPatch
     , "Property Checks" ~: propertyChecks
     , "Insert and Patch" ~: do
         let i :: PatchMapWithPatchingMove () (Sum Int)
             i = PatchMapWithPatchingMove.insertMapKey () 1
             p = PatchMapWithPatchingMove.patchMapKey () (Sum 2)
             pAfterI = PatchMapWithPatchingMove.insertMapKey () 3
         assertEqual "Insert after patch is the same as insert" (i <> p) i
         assertEqual "Patch after insert is a patched insert" (p <> i) pAfterI
     ]
   if errors counts + failures counts == 0 then exitSuccess else exitFailure

propertyChecks :: IO Bool
propertyChecks = checkParallel $$(discover)

prop_patchThatChangesMap :: Property
prop_patchThatChangesMap = property $ do
   mapBefore <- makeRandomIntMap
   mapAfter <- makeRandomIntMap
   let patch = patchThatChangesMap mapBefore mapAfter
   Just mapAfter === apply patch mapBefore

makeRandomIntMap :: Monad m => PropertyT m (Map Int Int)
makeRandomIntMap = do
   let genNum = Gen.int (Range.linear 0 100)
   length <- forAll genNum
   listOfNumbers <- forAll $ Seq.replicateM length genNum
   pure $ Seq.foldMapWithIndex Map.singleton listOfNumbers
