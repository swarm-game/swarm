{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: calculate the graph of craftable entities
-- and their dependencies via recipes.
module Swarm.Game.Recipe.Graph (
  RecipeGraphData (..),
  classicScenarioRecipeGraphData,
) where

import Control.Algebra (Has)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (view, (^.))
import Data.Map.Lazy (Map, (!))
import Data.Map.Lazy qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Swarm.Failure (SystemFailure (..), simpleErrorHandle)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityYields)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Land
import Swarm.Game.Recipe (Recipe, recipeCatalysts, recipeInputs, recipeOutputs)
import Swarm.Game.Robot (TRobot, tequippedDevices, trobotInventory)
import Swarm.Game.Scenario (GameStateInputs (..), ScenarioInputs (..), ScenarioLandscape, loadStandaloneScenario, scenarioLandscape, scenarioRobots)
import Swarm.Game.World.Gen (extractEntities)
import Swarm.Game.World.Typecheck (Some (..))
import Swarm.Util (both)

-- | A description of a recipe graph for a given scenario.
data RecipeGraphData = RecipeGraphData
  { rgWorldEntities :: Set Entity
  -- ^ Entities available in the world
  , rgStartingDevices :: Set Entity
  -- ^ Which devices the base starts with
  , rgStartingInventory :: Set Entity
  -- ^ What inventory the base starts with
  , rgLevels :: [Set Entity]
  -- ^ Recursively craftable entities, organized by distance from
  -- starting inventory (i.e. number of crafting steps needed)
  , rgAllEntities :: Set Entity
  -- ^ All known entities
  , rgRecipes :: [Recipe Entity]
  -- ^ All known recipes
  }

baseRobotTemplate :: Has (Throw SystemFailure) sig m => ScenarioLandscape -> m TRobot
baseRobotTemplate sLandscape = case listToMaybe $ view scenarioRobots sLandscape of
  Just r -> pure r
  Nothing -> throwError $ CustomFailure "Scenario contains no robots"

-- | Load the recipe graph corresponding to the classic scenario.
classicScenarioRecipeGraphData :: IO RecipeGraphData
classicScenarioRecipeGraphData = simpleErrorHandle $ do
  (classic, GameStateInputs (ScenarioInputs worlds (TerrainEntityMaps _ emap)) recipes) <-
    loadStandaloneScenario "data/scenarios/classic.yaml"
  baseRobot <- baseRobotTemplate (classic ^. scenarioLandscape)
  let classicTerm = worlds ! "classic"
  let devs = startingDevices baseRobot
  let inv = Map.keysSet $ startingInventory baseRobot
  let worldEntities = case classicTerm of Some _ t -> extractEntities t
  return
    RecipeGraphData
      { rgStartingDevices = devs
      , rgStartingInventory = inv
      , rgWorldEntities = worldEntities
      , rgLevels = recipeLevels emap recipes (Set.unions [worldEntities, devs, inv])
      , rgAllEntities = Set.fromList . Map.elems $ entitiesByName emap
      , rgRecipes = recipes
      }

scenarioRecipeGraphData :: Scenario -> ScenarioInputs -> [Recipe Entity] -> RecipeGraphData
scenarioRecipeGraphData scenario (ScenarioInputs words (TerrainEntityMaps _ emap)) recipes = do
  let landscape = scenario ^. scenarioLandscape
  baseRobot <- baseRobotTemplate landscape
  let worldEntities = landscapeEntities landscape
      devs = startingDevices baseRobot
      inv  = Map.keysSet $ startingInventory baseRobot
  return
    RecipeGraphData
      { rgStartingDevices = devs
      , rgStartingInventory = inv
      , rgWorldEntities = worldEntities
      , rgLevels = recipeLevels emap recipes (Set.unions [worldEntities, devs, inv])
      , rgAllEntities = Set.fromList . Map.elems $ entitiesByName emap
      , rgRecipes = recipes
      }

landscapeEntities :: ()
landscapeEntities = undefined

-------------------------------------------------------------------------------
-- RECIPE LEVELS
-------------------------------------------------------------------------------

-- | Order entities in sets depending on how soon it is possible to obtain them.
--
-- So:
--  * Level 0 - starting entities (for example those obtainable in the world)
--  * Level N+1 - everything possible to make (or drill or harvest) from Level N
--
-- This is almost a BFS, but the requirement is that the set of entities
-- required for recipe is subset of the entities known in Level N.
--
-- If we ever depend on some graph library, this could be rewritten
-- as some BFS-like algorithm with added recipe nodes, but you would
-- need to enforce the condition that recipes need ALL incoming edges.
recipeLevels :: EntityMap -> [Recipe Entity] -> Set Entity -> [Set Entity]
recipeLevels emap recipes start = levels
 where
  recipeParts r = ((r ^. recipeInputs) <> (r ^. recipeCatalysts), r ^. recipeOutputs)
  m :: [(Set Entity, Set Entity)]
  m = map (both (Set.fromList . map snd) . recipeParts) recipes
  levels :: [Set Entity]
  levels = reverse $ go [start] start
   where
    isKnown known (i, _o) = null $ i Set.\\ known
    lookupYield e = case view entityYields e of
      Nothing -> e
      Just yn -> case E.lookupEntityName yn emap of
        Nothing -> error "unknown yielded entity"
        Just ye -> ye
    yielded = Set.map lookupYield
    nextLevel known = Set.unions $ yielded known : map snd (filter (isKnown known) m)
    go ls known =
      let n = nextLevel known Set.\\ known
       in if null n
            then ls
            else go (n : ls) (Set.union n known)

startingDevices :: TRobot -> Set Entity
startingDevices = Set.fromList . map snd . E.elems . view tequippedDevices

startingInventory :: TRobot -> Map Entity Int
startingInventory = Map.fromList . map swap . E.elems . view trobotInventory
