{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensure recipe coverage for all entities that
-- grant capabilities (aka "devices").
module TestRecipeCoverage where

import Control.Lens ((^.))
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

testDeviceRecipeCoverage :: GameStateInputs -> TestTree
testDeviceRecipeCoverage gsi =
  testGroup
    "Recipe coverage"
    [ expectFailBecause "Need to come up with more recipes" checkCoverage
    ]
 where
  checkCoverage :: TestTree
  checkCoverage =
    testCase
      "Ensure all devices have recipes (#1268)"
      $ assertBool errMessage
      $ null nonCoveredEntities
   where
    errMessage =
      T.unpack $
        T.unwords
          [ "Missing recipes for:"
          , commaList $ map quote $ Set.toList nonCoveredEntities
          ]

    -- Only include entities that grant a capability:
    entityNames =
      Set.fromList . map ((^. entityName) . device) . concatMap NE.toList . M.elems . getMap . entitiesByCap $
        initEntityTerrain (gsiScenarioInputs gsi) ^. entityMap

    getOutputsForRecipe r = map ((^. entityName) . snd) $ r ^. recipeOutputs
    recipeOutputEntities = Set.fromList . concatMap getOutputsForRecipe $ gsiRecipes gsi
    nonCoveredEntities = Set.difference entityNames recipeOutputEntities
