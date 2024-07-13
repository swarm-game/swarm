{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensure recipe coverage for all entities.
module TestRecipeCoverage (testRecipeCoverage) where

import Control.Lens (view)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Doc.Gen
import Swarm.Game.Entity (Entity, EntityName, entityName)
import Swarm.Util (quote)
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

-- | Generate test tree to check that each entity either has a reachable
--   recipe or is in the world or starting base robot inventory.
--
-- If you added a recipe, thank you, please remove the entity from the list
-- of known uncraftable entities.
-- If you are not sure why an entity with a recipe is unreachable, check out
-- the dot graph of entity recipes in 'Swarm.Doc.Gen' that this test uses.
testRecipeCoverage :: IO TestTree
testRecipeCoverage = do
  graphData <- classicScenarioRecipeGraphData
  let sortE = List.sortOn (T.unpack . view entityName)
      allEntities = sortE . Set.toList $ rgAllEntities graphData
      nonCovered = getNonCoveredEntities graphData
  return . testGroup "Ensure all entities have recipes" $
    map (\e -> expectNonCovered e $ checkCoverage nonCovered e) allEntities
 where
  checkCoverage :: Set.Set Entity -> Entity -> TestTree
  checkCoverage s e =
    let name = view entityName e
     in testCase (T.unpack name) $ do
          assertBool (errMessage name) (name `elem` ignoredEntities || e `Set.notMember` s)
   where
    errMessage missing = T.unpack $ "Can not make " <> quote missing <> " from starting entities."

expectNonCovered :: Entity -> TestTree -> TestTree
expectNonCovered e =
  let name = T.toCaseFold (view entityName e)
   in if name `elem` nonCoveredList
        then expectFailBecause "More recipes needed (#1268)"
        else id

-- | Known non-covered entities that need a recipe.
nonCoveredList :: [EntityName]
nonCoveredList =
  map
    T.toCaseFold
    [ "Elmer's glue"
    , "ash"
    , "binoculars"
    , "blueprint"
    , "caliper"
    , "cup of tea"
    , "decoder ring"
    , "dozer blade"
    , "infinite improbability drive"
    , "linotype"
    , "olfactometer"
    , "rolex"
    , "tape drive"
    , "tea leaves"
    , "wedge"
    ]

getNonCoveredEntities :: RecipeGraphData -> Set.Set Entity
getNonCoveredEntities graphData = rgAllEntities graphData `Set.difference` Set.unions (rgLevels graphData)
