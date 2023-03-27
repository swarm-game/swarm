{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module Main where

import Control.Monad.Except (runExceptT)
import Data.List (subsequences)
import Data.Set (Set)
import Data.Set qualified as S
import Swarm.Game.State (GameState, classicGame0)
import Swarm.Util (smallHittingSet)
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
import TestEval (testEval)
import TestInventory (testInventory)
import TestLSP (testLSP)
import TestLanguagePipeline (testLanguagePipeline)
import TestModel (testModel)
import TestNotification (testNotification)
import TestPretty (testPrettyConst)
import Witch (from)

main :: IO ()
main = do
  mg <- runExceptT classicGame0
  case mg of
    Left err -> assertFailure (from err)
    Right g -> defaultMain (tests g)

tests :: GameState -> TestTree
tests g =
  testGroup
    "Tests"
    [ testLanguagePipeline
    , testPrettyConst
    , testBoolExpr
    , testCommands
    , testEval g
    , testModel
    , testInventory
    , testNotification g
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
