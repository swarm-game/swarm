-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
module Swarm.Game.State.Initialize (
  scenarioToGameState,
  pureScenarioToGameState,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Carrier.State.Lazy qualified as Fused
import Control.Effect.Lens (view)
import Control.Effect.Lift (Has)
import Control.Effect.State (State)
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Data.Foldable.Extra (allM)
import Data.IntMap qualified as IM
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set qualified as S
import Data.Text (Text)
import Linear (V2 (..))
import Swarm.Game.CESK (finalValue, initMachine)
import Swarm.Game.Device (getCapabilitySet, getMap)
import Swarm.Game.Entity
import Swarm.Game.Land
import Swarm.Game.Recipe (
  catRecipeMap,
  inRecipeMap,
  outRecipeMap,
 )
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Universe as U
import Swarm.Game.World.Gen (Seed)
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Syntax (allConst)
import Swarm.Language.Types
import Swarm.Util (binTuples, (?))
import System.Clock qualified as Clock
import System.Random (mkStdGen)

-- | Create an initial game state corresponding to the given scenario.
scenarioToGameState ::
  Scenario ->
  ValidatedLaunchParams ->
  GameStateConfig ->
  IO GameState
scenarioToGameState scenario (LaunchParams (Identity userSeed) (Identity toRun)) gsc = do
  theSeed <- arbitrateSeed userSeed $ scenario ^. scenarioLandscape
  now <- Clock.getTime Clock.Monotonic
  return $ pureScenarioToGameState scenario theSeed now toRun gsc

pureScenarioToGameState ::
  Scenario ->
  Seed ->
  Clock.TimeSpec ->
  Maybe CodeToRun ->
  GameStateConfig ->
  GameState
pureScenarioToGameState scenario theSeed now toRun gsc =
  preliminaryGameState
    & discovery . structureRecognition .~ recognizer
 where
  sLandscape = scenario ^. scenarioLandscape

  recognizer =
    runIdentity $
      Fused.evalState preliminaryGameState $
        mkRecognizer (sLandscape ^. scenarioStructures)

  gs = initGameState gsc
  preliminaryGameState =
    gs
      & robotInfo %~ setRobotInfo baseID robotList'
      & creativeMode .~ scenario ^. scenarioOperation . scenarioCreative
      & winCondition .~ theWinCondition
      & winSolution .~ scenario ^. scenarioOperation . scenarioSolution
      & discovery . availableCommands .~ Notifications 0 False initialCommands
      & discovery . knownEntities .~ sLandscape ^. scenarioKnown
      & discovery . tagMembers .~ buildTagMap em
      & randomness . seed .~ theSeed
      & randomness . randGen .~ mkStdGen theSeed
      & recipesInfo %~ modifyRecipesInfo
      & landscape .~ mkLandscape sLandscape worldTuples theSeed
      & gameControls . initiallyRunCode .~ initialCodeToRun
      & gameControls . replStatus .~ case running of -- When the base starts out running a program, the REPL status must be set to working,
      -- otherwise the store of definition cells is not saved (see #333, #838)
        False -> REPLDone Nothing
        True -> REPLWorking PolyUnit Nothing
      & temporal . robotStepsPerTick .~ ((scenario ^. scenarioOperation . scenarioStepsPerTick) ? defaultRobotStepsPerTick)

  robotList' = (robotCreatedAt .~ now) <$> robotList

  modifyRecipesInfo oldRecipesInfo =
    oldRecipesInfo
      & recipesOut %~ addRecipesWith outRecipeMap
      & recipesIn %~ addRecipesWith inRecipeMap
      & recipesCat %~ addRecipesWith catRecipeMap

  TerrainEntityMaps _ em = sLandscape ^. scenarioTerrainAndEntities
  baseID = 0
  (things, devices) = partition (M.null . getMap . view entityCapabilities) (M.elems (entitiesByName em))

  getCodeToRun (CodeToRun _ s) = s

  robotsByBasePrecedence = genRobotTemplates sLandscape worldTuples

  initialCodeToRun = getCodeToRun <$> toRun

  robotListRaw =
    zipWith (instantiateRobot Nothing) [baseID ..] robotsByBasePrecedence

  robotList =
    robotListRaw
      -- If the  --run flag was used, use it to replace the CESK machine of the
      -- robot whose id is 0, i.e. the first robot listed in the scenario.
      -- Note that this *replaces* any program the base robot otherwise
      -- would have run (i.e. any program specified in the program: field
      -- of the scenario description).
      & ix baseID
        . machine
        %~ case initialCodeToRun of
          Nothing -> id
          Just t -> const $ initMachine t
      -- If we are in creative mode, give base all the things
      & ix baseID
        . robotInventory
        %~ case scenario ^. scenarioOperation . scenarioCreative of
          False -> id
          True -> union (fromElems (map (0,) things))
      & ix baseID
        . equippedDevices
        %~ case scenario ^. scenarioOperation . scenarioCreative of
          False -> id
          True -> const (fromList devices)

  running = case robotList of
    [] -> False
    (base : _) -> isNothing (finalValue (base ^. machine))

  -- Initial list of available commands = all commands enabled by
  -- devices in inventory or equipped; and commands that require no
  -- capability.
  allCapabilities r =
    inventoryCapabilities (r ^. equippedDevices)
      <> inventoryCapabilities (r ^. robotInventory)
  initialCaps = getCapabilitySet $ mconcat $ map allCapabilities robotList
  initialCommands =
    filter
      (maybe True (`S.member` initialCaps) . constCaps)
      allConst

  worldTuples = buildWorldTuples sLandscape

  theWinCondition =
    maybe
      NoWinCondition
      (WinConditions Ongoing . initCompletion . NE.toList)
      (NE.nonEmpty (scenario ^. scenarioOperation . scenarioObjectives))

  addRecipesWith f = IM.unionWith (<>) (f $ scenario ^. scenarioOperation . scenarioRecipes)

mkRecognizer ::
  (Has (State GameState) sig m) =>
  StaticStructureInfo ->
  m (StructureRecognizer StructureCells Entity)
mkRecognizer structInfo@(StaticStructureInfo structDefs _) = do
  foundIntact <- mapM (sequenceA . (id &&& ensureStructureIntact)) allPlaced
  let fs = populateStaticFoundStructures . map fst . filter snd $ foundIntact
  return $
    StructureRecognizer
      (mkAutomatons structDefs)
      fs
      [IntactStaticPlacement $ map mkLogEntry foundIntact]
 where
  allPlaced = lookupStaticPlacements structInfo
  mkLogEntry (x, intact) =
    IntactPlacementLog
      intact
      ((getName . originalDefinition . structureWithGrid) x)
      (upperLeftCorner x)

-- | Matches definitions against the placements.
-- Fails fast (short-circuits) if a non-matching
-- cell is encountered.
ensureStructureIntact ::
  (Has (State GameState) sig m) =>
  FoundStructure StructureCells Entity ->
  m Bool
ensureStructureIntact (FoundStructure (StructureWithGrid _ _ grid) upperLeft) =
  allM outer $ zip [0 ..] grid
 where
  outer (y, row) = allM (inner y) $ zip [0 ..] row
  inner y (x, maybeTemplateEntity) = case maybeTemplateEntity of
    Nothing -> return True
    Just _ ->
      fmap (== maybeTemplateEntity) $
        entityAt $
          upperLeft `offsetBy` V2 x (negate y)

buildTagMap :: EntityMap -> Map Text (NonEmpty EntityName)
buildTagMap em =
  binTuples expanded
 where
  expanded = concatMap (\(k, vs) -> [(v, k) | v <- S.toList vs]) tagsByEntity
  tagsByEntity = map (view entityName &&& view entityTags) $ entityDefinitionOrder em
