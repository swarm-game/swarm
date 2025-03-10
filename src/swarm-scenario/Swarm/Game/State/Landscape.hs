{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.State.Landscape (
  Landscape,
  SubworldDescription,

  -- ** Lenses
  worldNavigation,
  multiWorld,
  worldScrollable,
  terrainAndEntities,
  recognizerAutomatons,

  -- ** Utilities
  initLandscape,
  mkLandscape,
  buildWorldTuples,
  genMultiWorld,
  buildWorld,
  genRobotTemplates,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Lens hiding (Const, both, use, uses, (%=), (+=), (.=), (<+=), (<<.=))
import Data.Array (Array, listArray)
import Data.Bifunctor (first)
import Data.Int (Int32)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Tuple.Extra (both, swap)
import Swarm.Game.Entity
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Robot (TRobot, trobotLocation)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.RobotLookup (IndexedTRobot)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Navigation.Portal (Navigation (..))
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.State.Config
import Swarm.Game.Terrain (TerrainType (..), terrainIndexByName)
import Swarm.Game.Universe as U
import Swarm.Game.World
import Swarm.Game.World.Coords
import Swarm.Game.World.Eval (runWorld)
import Swarm.Game.World.Gen (Seed)
import Swarm.Util.Erasable
import Swarm.Util.Lens (makeLensesNoSigs)

type SubworldDescription = (SubworldName, ([IndexedTRobot], Seed -> WorldFun Int Entity))

data Landscape = Landscape
  { _worldNavigation :: Navigation (M.Map SubworldName) Location
  , _multiWorld :: MultiWorld Int Entity
  , _terrainAndEntities :: TerrainEntityMaps
  , _recognizerAutomatons :: RecognizerAutomatons RecognizableStructureContent Entity
  , _worldScrollable :: Bool
  }

makeLensesNoSigs ''Landscape

-- | Includes a 'Map' of named locations and an
-- "edge list" (graph) that maps portal entrances to exits
worldNavigation :: Lens' Landscape (Navigation (M.Map SubworldName) Location)

-- | The current state of the world (terrain and entities only; robots
--   are stored in the 'robotMap').  'Int' is used instead of
--   'TerrainType' because we need to be able to store terrain values in
--   unboxed tile arrays.
multiWorld :: Lens' Landscape (MultiWorld Int Entity)

-- | The catalogs of all terrain and entities that the game knows about.
terrainAndEntities :: Lens' Landscape TerrainEntityMaps

-- | Recognition engine for predefined structures
recognizerAutomatons :: Lens' Landscape (RecognizerAutomatons RecognizableStructureContent Entity)

-- | Whether the world map is supposed to be scrollable or not.
worldScrollable :: Lens' Landscape Bool

-- | Create an record that is empty except for
-- system-provided entities.
initLandscape :: GameStateConfig -> Landscape
initLandscape gsc =
  Landscape
    { _worldNavigation = Navigation mempty mempty
    , _multiWorld = mempty
    , _terrainAndEntities = initEntityTerrain $ gsiScenarioInputs $ initState gsc
    , _recognizerAutomatons = RecognizerAutomatons mempty mempty
    , _worldScrollable = True
    }

mkLandscape :: ScenarioLandscape -> NonEmpty SubworldDescription -> Seed -> Landscape
mkLandscape sLandscape worldTuples theSeed =
  Landscape
    { _worldNavigation = sLandscape ^. scenarioNavigation
    , _multiWorld = genMultiWorld worldTuples theSeed
    , _terrainAndEntities = sLandscape ^. scenarioTerrainAndEntities
    , _recognizerAutomatons = sLandscape ^. scenarioStructures . staticAutomatons
    , -- TODO (#1370): Should we allow subworlds to have their own scrollability?
      -- Leaning toward no, but for now just adopt the root world scrollability
      -- as being universal.
      _worldScrollable = NE.head (sLandscape ^. scenarioWorlds) ^. to scrollable
    }

buildWorldTuples :: ScenarioLandscape -> NonEmpty SubworldDescription
buildWorldTuples sLandscape =
  NE.map (worldName &&& buildWorld (sLandscape ^. scenarioTerrainAndEntities)) $
    sLandscape ^. scenarioWorlds

genMultiWorld :: NonEmpty SubworldDescription -> Seed -> MultiWorld Int Entity
genMultiWorld worldTuples s =
  M.map genWorld
    . M.fromList
    . NE.toList
    $ worldTuples
 where
  genWorld x = newWorld $ snd x s

-- | Take a world description, parsed from a scenario file, and turn
--   it into a list of located robots and a world function.
buildWorld ::
  TerrainEntityMaps ->
  WorldDescription ->
  ([IndexedTRobot], Seed -> WorldFun Int Entity)
buildWorld tem WorldDescription {..} =
  (robots worldName, first getTerrainIndex . wf)
 where
  getTerrainIndex t =
    M.findWithDefault 0 t $
      terrainIndexByName $
        tem ^. terrainMap

  g = gridContent area

  worldGrid :: Grid (TerrainType, Erasable Entity)
  worldGrid = maybe (BlankT, ENothing) (cellTerrain &&& cellEntity) <$> g

  offsetCoordsByArea :: Coords -> AreaDimensions -> Coords
  offsetCoordsByArea x a =
    x `addTuple` swap (asTuple a)

  coords = locToCoords $ gridPosition area

  arrayMaxBound =
    both (subtract 1)
      . unCoords
      . offsetCoordsByArea coords
      $ getGridDimensions g

  arrayBoundsTuple = (unCoords coords, arrayMaxBound)

  worldArray :: Array (Int32, Int32) (TerrainType, Erasable Entity)
  worldArray = listArray arrayBoundsTuple $ allMembers worldGrid

  dslWF, arrayWF :: Seed -> WorldFun TerrainType Entity
  dslWF = maybe mempty runWorld worldProg
  arrayWF = const $ worldFunFromArray worldArray

  wf = dslWF <> arrayWF

  -- Get all the robots described in cells and set their locations appropriately
  robots :: SubworldName -> [IndexedTRobot]
  robots swName =
    concat $ mapWithCoords extractRobots g
   where
    extractRobots (Coords coordsTuple) maybeCell =
      let robotWithLoc = trobotLocation ?~ Cosmic swName (coordsToLoc (coords `addTuple` coordsTuple))
       in map (fmap robotWithLoc) (maybe [] cellRobots maybeCell)

-- |
-- Returns a list of robots, ordered by decreasing preference
-- to serve as the "base".
--
-- = Rules for selecting the "base" robot:
--
-- What follows is a thorough description of how the base
-- choice is made as of the most recent study of the code.
-- This level of detail is not meant to be public-facing.
--
-- For an abbreviated explanation, see the "Base robot" section of the
-- <https://github.com/swarm-game/swarm/tree/main/data/scenarios#base-robot Scenario Authoring Guide>.
--
-- == Precedence rules
--
-- 1. Prefer those robots defined with a @loc@ ('robotLocation') in the scenario file
--
--     1. If multiple robots define a @loc@, use the robot that is defined
--        first within the scenario file.
--     2. Note that if a robot is both given a @loc@ AND is specified in the
--        world map, then two instances of the robot shall be created. The
--        instance with the @loc@ shall be preferred as the base.
--
-- 2. Fall back to robots generated from templates via the map and palette.
--
--     1. If multiple robots are specified in the map, prefer the one that
--        is defined first within the scenario file.
--     2. If multiple robots are instantiated from the same template, then
--        prefer the one with a lower-indexed subworld. Note that the root
--        subworld is always first.
--     3. If multiple robots instantiated from the same template are in the
--        same subworld, then
--        prefer the one closest to the upper-left of the screen, with higher
--        rows given precedence over columns (i.e. first in row-major order).
genRobotTemplates :: ScenarioLandscape -> NonEmpty (a, ([(Int, TRobot)], b)) -> [TRobot]
genRobotTemplates sLandscape worldTuples =
  locatedRobots ++ map snd (sortOn fst genRobots)
 where
  -- Keep only robots from the robot list with a concrete location;
  -- the others existed only to serve as a template for robots drawn
  -- in the world map
  locatedRobots = filter (isJust . view trobotLocation) $ sLandscape ^. scenarioRobots

  -- Subworld order as encountered in the scenario YAML file is preserved for
  -- the purpose of numbering robots, other than the "root" subworld
  -- guaranteed to be first.
  genRobots :: [(Int, TRobot)]
  genRobots = concat $ NE.toList $ NE.map (fst . snd) worldTuples
