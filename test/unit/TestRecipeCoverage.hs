{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensure recipe coverage for all entities that
-- grant capabilities (aka "devices").
module TestRecipeCoverage where

import Control.Lens ((^.), view)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Game.Device
import Swarm.Game.Entity (EntityMap (entitiesByCap), entityName)
import Swarm.Game.Land
import Swarm.Game.Recipe (recipeOutputs)
import Swarm.Game.Scenario (GameStateInputs (..), initEntityTerrain)
import Swarm.Util (commaList, quote)
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit
import Swarm.Doc.Gen

testDeviceRecipeCoverage :: TestTree
testDeviceRecipeCoverage =
  testGroup
    "Recipe coverage"
    [ expectFailBecause "Need to come up with more recipes" checkCoverage
    ]
 where
  checkCoverage :: TestTree
  checkCoverage =
    testCase "Ensure all devices have recipes (#1268)" $ do
      graphData <- classicScenarioRecipeGraphData
      let nonCoveredEntities = filter (\e -> view entityName e `notElem` ignoredEntities) . Set.toList $
            rgAllEntities graphData `Set.difference` Set.unions (rgLevels graphData)
      assertBool (errMessage nonCoveredEntities) (null nonCoveredEntities)
   where
    errMessage missing = T.unpack $ "Missing recipes for: " <> commaList (quote . view entityName <$> missing)
