{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensure recipe coverage for all entities.
module TestRecipeCoverage where

import Control.Lens (view)
import Data.Function ((&))
import Data.List (intercalate, sort)
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Doc.Gen
import Swarm.Game.Entity (entityName)
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

testRecipeCoverage :: TestTree
testRecipeCoverage =
  expectFailBecause "Need to come up with more recipes" checkCoverage
 where
  checkCoverage :: TestTree
  checkCoverage =
    testCase "Ensure all devices have recipes (#1268)" $ do
      graphData <- classicScenarioRecipeGraphData
      let nonCoveredEntities =
            Set.unions (rgLevels graphData)
              & Set.difference (rgAllEntities graphData)
              & Set.toList
              & map (view entityName)
              & filter (`notElem` ignoredEntities)
              & map T.unpack
              & sort -- Text and String give different sort
      assertBool (errMessage nonCoveredEntities) (null nonCoveredEntities)
   where
    errMessage missing = "Missing recipes for: " <> intercalate ", " (quote <$> missing)
    quote t = concat ["\"", t, "\""]
