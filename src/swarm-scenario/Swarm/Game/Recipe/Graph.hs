{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: calculate the graph of craftable entities
-- and their dependencies via recipes.
module Swarm.Game.Recipe.Graph (
  RecipeGraph (..),
  classicScenarioRecipeGraph,
  scenarioRecipeGraph,
  ignoredEntities,
) where

import Control.Lens (view, (^.))
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Tuple (swap)
import Swarm.Failure (simpleErrorHandle)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), EntityProperty (Pickable), entityProperties, entityYields)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Land
import Swarm.Game.Recipe (Recipe, recipeCatalysts, recipeInputs, recipeOutputs)
import Swarm.Game.Robot (TRobot, tequippedDevices, trobotInventory)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Cell (PCell (..))
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Overlay (gridContent)
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.World.Gen (extractEntities)
import Swarm.Util (both)
import Swarm.Util.Erasable (erasableToMaybe)

-- | Ignore utility entities that are just used for tutorials and challenges.
ignoredEntities :: Set Text
ignoredEntities = Set.fromList ["wall"]

-- | A description of a recipe graph for a given scenario.
data RecipeGraph = RecipeGraph
  { worldEntities :: Set Entity
  -- ^ Entities available in the world
  , startingDevices :: Set Entity
  -- ^ Which devices the base starts with
  , startingInventory :: Set Entity
  -- ^ What inventory the base starts with
  , levels :: [Set Entity]
  -- ^ Recursively craftable entities, organized by distance from
  -- starting inventory (i.e. number of crafting steps needed)
  , allEntities :: Set Entity
  -- ^ All known entities
  , recipes :: [Recipe Entity]
  -- ^ All known recipes
  }

baseRobotTemplate :: ScenarioLandscape -> Maybe TRobot
baseRobotTemplate = listToMaybe . view scenarioRobots

-- | Load the recipe graph corresponding to the classic scenario.
classicScenarioRecipeGraph :: IO RecipeGraph
classicScenarioRecipeGraph = simpleErrorHandle $ do
  (classic, gsi) <- loadStandaloneScenario "data/scenarios/classic.yaml"
  pure $ scenarioRecipeGraph classic gsi

scenarioRecipeGraph :: Scenario -> GameStateInputs -> RecipeGraph
scenarioRecipeGraph scenario (GameStateInputs (ScenarioInputs _ (TerrainEntityMaps _ emap)) recipeList) =
  RecipeGraph
    { startingDevices = devs
    , startingInventory = inv
    , worldEntities = ents
    , levels = recipeLevels scenarioEMap recipeList (Set.unions [ents, devs, inv])
    , allEntities = Set.fromList . Map.elems $ entitiesByName scenarioEMap
    , recipes = recipeList ++ (scenario ^. scenarioOperation . scenarioRecipes)
    }
 where
  scenarioEMap = (scenario ^. scenarioLandscape . scenarioTerrainAndEntities . entityMap) <> emap
  landscape = scenario ^. scenarioLandscape
  baseRobot = baseRobotTemplate landscape
  devs = maybe Set.empty robotStartingDevices baseRobot
  inv = maybe Set.empty (Map.keysSet . robotStartingInventory) baseRobot
  ents = landscapeEntities landscape

-- | Get the set of all entities which can be harvested from the world.
landscapeEntities :: ScenarioLandscape -> Set Entity
landscapeEntities = foldMap harvestable . view scenarioWorlds
 where
  harvestable :: WorldDescription -> Set Entity
  harvestable wd = Set.fromList (static wd) <> dynamic wd
   where
    static :: WorldDescription -> [Entity]
    static =
      filter (Set.member Pickable . view entityProperties)
        . mapMaybe (erasableToMaybe . cellEntity)
        . catMaybes
        . allMembers
        . gridContent
        . area

    dynamic :: WorldDescription -> Set Entity
    dynamic = maybe Set.empty extractEntities . worldProg

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
recipeLevels emap recipeList start = levs
 where
  recipeParts r = ((r ^. recipeInputs) <> (r ^. recipeCatalysts), r ^. recipeOutputs)
  m :: [(Set Entity, Set Entity)]
  m = map (both (Set.fromList . map snd) . recipeParts) recipeList
  levs :: [Set Entity]
  levs = reverse $ go [start] start
   where
    isKnown known (i, _o) = null $ i Set.\\ known
    lookupYield e = case view entityYields e of
      Nothing -> e
      Just yn -> fromMaybe e (E.lookupEntityName yn emap)
    yielded = Set.map lookupYield
    nextLevel known = Set.unions $ yielded known : map snd (filter (isKnown known) m)
    go ls known =
      let n = nextLevel known Set.\\ known
       in if null n
            then ls
            else go (n : ls) (Set.union n known)

robotStartingDevices :: TRobot -> Set Entity
robotStartingDevices = Set.fromList . map snd . E.elems . view tequippedDevices

robotStartingInventory :: TRobot -> Map Entity Int
robotStartingInventory = Map.fromList . map swap . E.elems . view trobotInventory
