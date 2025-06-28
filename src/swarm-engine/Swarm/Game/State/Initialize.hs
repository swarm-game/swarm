-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
module Swarm.Game.State.Initialize (
  scenarioToGameState,
  scenarioToGameStateForTests,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Carrier.State.Strict qualified as Fused
import Control.Effect.Lens (view)
import Control.Lens hiding (view)
import Data.Hashable (Hashable)
import Data.IntMap qualified as IM
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Tuple.Extra (dupe)
import Swarm.Effect qualified as Effect
import Swarm.Game.CESK (finalValue, initMachine)
import Swarm.Game.Device (getCapabilitySet, getMap)
import Swarm.Game.Entity
import Swarm.Game.Land
import Swarm.Game.Recipe (
  catRecipeMap,
  inRecipeMap,
  outRecipeMap,
 )
import Swarm.Game.Recipe.Graph qualified as RG
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Objective (initCompletion)
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (emptyFoundStructures)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.State.Landscape (mkLandscape, worldMetrics)
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Game.World (Seed, WorldMetrics, initWorldMetrics)
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Syntax (allConst)
import Swarm.Language.Types
import Swarm.Util (applyWhen, binTuples, (?))
import System.Clock qualified as Clock
import System.Random (mkStdGen)

-- | Create an initial game state corresponding to the given scenario.
scenarioToGameState ::
  ScenarioWith (Maybe ScenarioPath) ->
  ValidatedLaunchParams ->
  Maybe GameMetrics ->
  Maybe WorldMetrics ->
  RuntimeState ->
  IO GameState
scenarioToGameState si (LaunchParams (Identity userSeed) (Identity toRun)) prevGMetric prevWMetric rs = do
  theSeed <- arbitrateSeed userSeed land
  now <- Clock.getTime Clock.Monotonic
  gMetric <- maybe (initGameMetrics $ rs ^. metrics) pure prevGMetric
  wMetric <- maybe (initWorldMetrics $ rs ^. metrics) pure prevWMetric
  -- set up game state
  let gs = pureScenarioToGameState si theSeed now toRun (Just gMetric) (Just wMetric) (rs ^. stdGameConfigInputs)
  -- then initialize structure recognition (needs IO because of the metrics)
  (gs', recognition) <- runEffects gs $ initializeRecognition entityAt (land ^. scenarioStructures)
  return $ gs' & discovery . structureRecognition .~ recognition
 where
  land = si ^. getScenario . scenarioLandscape
  runEffects gs = Fused.runState gs . Effect.runMetricIO . Effect.runTimeIO

-- | This initialises the 'GameState' without running metrics.
scenarioToGameStateForTests ::
  ScenarioWith (Maybe ScenarioPath) ->
  Seed ->
  Clock.TimeSpec ->
  Maybe CodeToRun ->
  GameStateConfig ->
  GameState
scenarioToGameStateForTests si theSeed now toRun gsc =
  gs'
    & discovery . structureRecognition .~ recognition
 where
  gs = pureScenarioToGameState si theSeed now toRun Nothing Nothing gsc
  structures = si ^. getScenario . scenarioLandscape . scenarioStructures
  (gs', recognition) = runPureEffects $ initializeRecognition entityAt structures
  runPureEffects = Fused.run . Fused.runState gs . Effect.runFakeMetric . Effect.runFakeTime 0

-- | Initialize the GameState record for current scenario.
pureScenarioToGameState ::
  ScenarioWith (Maybe ScenarioPath) ->
  Seed ->
  Clock.TimeSpec ->
  Maybe CodeToRun ->
  Maybe GameMetrics ->
  Maybe WorldMetrics ->
  GameStateConfig ->
  GameState
pureScenarioToGameState (ScenarioWith scenario fp) theSeed now toRun gMetric wMetric gsc =
  initGameState gsc
    & currentScenarioPath .~ fp
    & robotInfo %~ setRobotInfo baseID robotList'
    & creativeMode .~ scenario ^. scenarioOperation . scenarioCreative
    & winCondition .~ theWinCondition
    & winSolution .~ scenario ^. scenarioOperation . scenarioSolution
    & discovery . availableCommands .~ Notifications 0 False initialCommands
    & discovery . knownEntities .~ sLandscape ^. scenarioKnown
    & discovery . tagMembers .~ buildTagMap em
    & discovery . craftableDevices .~ craftable
    & randomness . seed .~ theSeed
    & randomness . randGen .~ mkStdGen theSeed
    & recipesInfo %~ modifyRecipesInfo
    & landscape .~ mkLandscape sLandscape worldTuples theSeed
    & landscape . worldMetrics .~ wMetric
    & gameControls . initiallyRunCode .~ initialCodeToRun
    & gameControls . replStatus .~ initialReplStatus
    & gameMetrics .~ gMetric
    & temporal . robotStepsPerTick .~ ((scenario ^. scenarioOperation . scenarioStepsPerTick) ? defaultRobotStepsPerTick)
 where
  sLandscape = scenario ^. scenarioLandscape
  robotList' = (robotCreatedAt .~ now) <$> robotList

  -- When the base starts out running a program, the REPL status must be set to working,
  -- otherwise the store of definition cells is not saved (see #333, #838)
  initialReplStatus = case running of
    False -> REPLDone Nothing
    True -> REPLWorking PolyUnit Nothing

  -- Get the names of all devices (i.e. entities that provide at least
  -- one capability) which can be recursively crafted from the
  -- starting inventory + entities available in the world
  craftable = S.map (view entityName) . S.filter isDevice $ S.unions (RG.levels recipeGraph)
   where
    recipeGraph = RG.scenarioRecipeGraph scenario (initState gsc)
    isDevice :: Entity -> Bool
    isDevice = not . M.null . getMap . view entityCapabilities

  modifyRecipesInfo oldRecipesInfo =
    oldRecipesInfo
      & recipesOut %~ addRecipesWith outRecipeMap
      & recipesIn %~ addRecipesWith inRecipeMap
      & recipesCat %~ addRecipesWith catRecipeMap

  TerrainEntityMaps _ em = sLandscape ^. scenarioTerrainAndEntities
  baseID = 0
  (things, devices) = partition (M.null . getMap . view entityCapabilities) (M.elems (entitiesByName em))

  robotsByBasePrecedence = genRobotTemplates sLandscape worldTuples

  initialCodeToRun = view toRunSyntax <$> toRun

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
        %~ applyWhen
          (scenario ^. scenarioOperation . scenarioCreative)
          (union (fromElems (map (0,) things)))
      & ix baseID
        . equippedDevices
        %~ applyWhen
          (scenario ^. scenarioOperation . scenarioCreative)
          (const (fromList devices))

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

-- |
-- As part of initializing the recognizer, we also pre-populate the
-- list of "found" structures with those statically placed by the scenario definition.
-- Note that this bypasses the regular "online" recognition machinery;
-- we don't actually have to "search" for these structures since we are
-- explicitly given their location; we only need to validate that each
-- structure remains intact given other, potentially overlapping static placements.
--
-- It may be possible at some point for the game seed to affect whether
-- initially-placed structures remain intact, by way of random placements.
-- Therefore we run this at 'GameState' initialization time, rather than
-- 'Scenario' parse time.
initializeRecognition ::
  (Monad s, Hashable a) =>
  GenericEntLocator s a ->
  StaticStructureInfo a b ->
  s (RecognitionState a b)
initializeRecognition entLoader structInfo = do
  foundIntact <- mapM checkIntactness allPlaced

  let fs = populateStaticFoundStructures . map fst . filter (null . snd) $ foundIntact
  return $
    RecognitionState
      fs
      [IntactStaticPlacement $ map mkLogEntry foundIntact]
 where
  checkIntactness = traverse (ensureStructureIntact emptyFoundStructures entLoader) . dupe

  allPlaced = lookupStaticPlacements structInfo
  mkLogEntry (x, intact) =
    IntactPlacementLog
      intact
      $ PositionedStructure (upperLeftCorner x) ((distillLabel . structureWithGrid) x)

buildTagMap :: EntityMap -> Map Text (NonEmpty EntityName)
buildTagMap em =
  binTuples expanded
 where
  expanded = concatMap (\(k, vs) -> [(v, k) | v <- S.toList vs]) tagsByEntity
  tagsByEntity = map (view entityName &&& view entityTags) $ entityDefinitionOrder em
