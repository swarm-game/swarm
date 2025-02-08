{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module Main where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Data.List (subsequences)
import Data.Set (Set)
import Data.Set qualified as S
import Swarm.TUI.Model (AppState, gameState, runtimeState)
import Swarm.TUI.Model.StateUpdate (classicGame0)
import Swarm.Util (removeSupersets, smallHittingSet)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Poly qualified as QC
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.QuickCheck (
  Arbitrary (arbitrary),
  Property,
  testProperty,
  (==>),
 )
import TestBoolExpr (testBoolExpr)
import TestCommand (testCommands)
import TestContext (testContext)
import TestEval (testEval)
import TestInventory (testInventory)
import TestLSP (testLSP)
import TestLanguagePipeline (testLanguagePipeline)
import TestNotification (testNotification)
import TestOrdering (testOrdering)
import TestOverlay (testOverlay)
import TestParse (testParse)
import TestPedagogy (testPedagogy)
import TestPretty (testPrettyConst)
import TestRepl (testRepl)
import TestRequirements (testRequirements)
import TestScoring (testHighScores)
import Witch (from)

main :: IO ()
main = do
  ms <- runExceptT classicGame0
  case ms of
    Left err -> assertFailure (from err)
    Right s -> defaultMain (tests s)

tests :: AppState -> TestTree
tests s =
  testGroup
    "Tests"
    [ statelessTests
    , stateDependentTests s
    ]

-- | Initializing an 'AppState' entails loading challenge scenarios, etc. from
-- disk.  We might not want to do this, in case we inject a 'trace' somewhere
-- in the scenario loading code and want to minimize the noise.
--
-- So we keep this list separate from the stateless tests so we can easily
-- comment it out.
stateDependentTests :: AppState -> TestTree
stateDependentTests s =
  testGroup
    "Stateful tests"
    [ testEval (s ^. gameState)
    , testPedagogy (s ^. runtimeState)
    , testNotification (s ^. gameState)
    ]

statelessTests :: TestTree
statelessTests =
  testGroup
    "Stateless tests"
    [ testLanguagePipeline
    , testParse
    , testPrettyConst
    , testBoolExpr
    , testCommands
    , testContext
    , testHighScores
    , testRepl
    , testRequirements
    , testInventory
    , testOrdering
    , testOverlay
    , testMisc
    , testLSP
    ]

testMisc :: TestTree
testMisc =
  testGroup
    "Miscellaneous"
    [ testProperty
        "smallHittingSet produces hitting sets"
        (prop_hittingSet @QC.OrdA)
    , testGroup
        "removeSupersets"
        [ testProperty
            "no two output sets are in a subset relation"
            (prop_removeSupersets_unrelated @QC.OrdA)
        , testProperty
            "all input sets are a superset of some output set"
            (prop_removeSupersets_all_inputs @QC.OrdA)
        ]
    ]

prop_hittingSet :: Ord a => [Set a] -> Property
prop_hittingSet ss = not (any S.null ss) ==> isHittingSet (smallHittingSet ss) ss

isHittingSet :: (Foldable t, Ord a) => Set a -> t (Set a) -> Bool
isHittingSet hs = not . any (S.null . S.intersection hs)

-- This property does *not* hold (and should not, because the problem
-- of producing a minimal hitting set is NP-hard), but we can use it
-- to generate counterexamples.
prop_hittingSetMinimal :: [Set El] -> Property
prop_hittingSetMinimal ss = not (any S.null ss) ==> all ((S.size hs <=) . S.size) allHittingSets
 where
  allElts = S.unions ss
  allSubsets = map S.fromList . subsequences . S.toList $ allElts
  allHittingSets = filter (`isHittingSet` ss) allSubsets
  hs = smallHittingSet ss

-- Five elements seem to be the minimum necessary for a
-- counterexample, but providing 6 helps QuickCheck find a
-- counterexample much more quickly.
data El = AA | BB | CC | DD | EE | FF
  deriving (Eq, Ord, Show, Bounded, Enum)

instance QC.Arbitrary El where
  arbitrary = QC.arbitraryBoundedEnum

prop_removeSupersets_unrelated :: Ord a => Set (Set a) -> Bool
prop_removeSupersets_unrelated (removeSupersets -> ss) =
  (`all` ss) $ \s1 ->
    (`all` ss) $ \s2 ->
      (s1 == s2) || (not (s1 `S.isSubsetOf` s2) && not (s2 `S.isSubsetOf` s1))

prop_removeSupersets_all_inputs :: Ord a => Set (Set a) -> Bool
prop_removeSupersets_all_inputs (removeSupersets -> ss) =
  (`all` ss) $ \s1 ->
    (`any` ss) $ \s2 -> s2 `S.isSubsetOf` s1
