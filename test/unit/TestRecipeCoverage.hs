{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensure recipe coverage for all entities that
-- grant capabilities (aka "devices").
module TestRecipeCoverage where

import Control.Lens ((^.))
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Game.Entity (EntityMap (entitiesByCap), entityName)
import Swarm.Game.Recipe (recipeOutputs)
import Swarm.Game.State.Runtime (RuntimeState, stdEntityTerrainMap, stdRecipes)
import Swarm.Util (commaList, quote)
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

testDeviceRecipeCoverage :: RuntimeState -> TestTree
testDeviceRecipeCoverage rs =
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
    entityNames = Set.fromList . map (^. entityName) . concat . M.elems . entitiesByCap $ rs ^. stdEntityTerrainMap . entityMap

    getOutputsForRecipe r = map ((^. entityName) . snd) $ r ^. recipeOutputs
    recipeOutputEntities = Set.fromList . concatMap getOutputsForRecipe $ rs ^. stdRecipes
    nonCoveredEntities = Set.difference entityNames recipeOutputEntities
