-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: calculate the graph of craftable entities
-- and their dependencies via recipes.
module Swarm.Game.Recipe.Graph (
  RecipeGraph (..),
  classicScenarioRecipeGraph,
  scenarioRecipeGraph,
) where

import Control.Lens (view, (^.))
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Swarm.Failure (simpleErrorHandle)
import Swarm.Game.Entity (Entity, EntityMap (entitiesByName), entityProperties, entityYields, EntityProperty(Pickable))
import Swarm.Game.Entity qualified as E
import Swarm.Game.Land
import Swarm.Game.Recipe (Recipe, recipeCatalysts, recipeInputs, recipeOutputs)
import Swarm.Game.Robot (TRobot, tequippedDevices, trobotInventory)
import Swarm.Game.Scenario (GameStateInputs (..), Scenario, ScenarioInputs (..), ScenarioLandscape, loadStandaloneScenario, scenarioLandscape, scenarioRobots, scenarioWorlds)
import Swarm.Game.Scenario.Topography.Cell (PCell (..))
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Overlay (gridContent)
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.World.Gen (extractEntities)
import Swarm.Util (both)
import Swarm.Util.Erasable (erasableToMaybe)

-- | A description of a recipe graph for a given scenario.
data RecipeGraph = RecipeGraph
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

baseRobotTemplate :: ScenarioLandscape -> Maybe TRobot
baseRobotTemplate = listToMaybe . view scenarioRobots

-- | Load the recipe graph corresponding to the classic scenario.
classicScenarioRecipeGraph :: IO RecipeGraph
classicScenarioRecipeGraph = simpleErrorHandle $ do
  (classic, gsi) <- loadStandaloneScenario "data/scenarios/classic.yaml"
  pure $ scenarioRecipeGraph classic gsi

-- baseRobot <- baseRobotTemplate (classic ^. scenarioLandscape)
-- let classicTerm = worlds ! "classic"
-- let devs = startingDevices baseRobot
-- let inv = Map.keysSet $ startingInventory baseRobot
-- let worldEntities = case classicTerm of Some _ t -> extractEntities t
-- return
--   RecipeGraph
--     { rgStartingDevices = devs
--     , rgStartingInventory = inv
--     , rgWorldEntities = worldEntities
--     , rgLevels = recipeLevels emap recipes (Set.unions [worldEntities, devs, inv])
--     , rgAllEntities = Set.fromList . Map.elems $ entitiesByName emap
--     , rgRecipes = recipes
--     }

scenarioRecipeGraph :: Scenario -> GameStateInputs -> RecipeGraph
scenarioRecipeGraph scenario (GameStateInputs (ScenarioInputs _ (TerrainEntityMaps _ emap)) recipes) =
  RecipeGraph
    { rgStartingDevices = devs
    , rgStartingInventory = inv
    , rgWorldEntities = worldEntities
    , rgLevels = recipeLevels emap recipes (Set.unions [worldEntities, devs, inv])
    , rgAllEntities = Set.fromList . Map.elems $ entitiesByName emap
    , rgRecipes = recipes
    }
 where
  landscape = scenario ^. scenarioLandscape
  -- Run Throw computation + turn into Either
  baseRobot = baseRobotTemplate landscape
  worldEntities = landscapeEntities landscape
  devs = maybe Set.empty startingDevices baseRobot
  inv = maybe Set.empty (Map.keysSet . startingInventory) baseRobot

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
