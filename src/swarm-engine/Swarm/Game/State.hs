{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
module Swarm.Game.State (
  -- * Game state record
  GameState,
  creativeMode,
  winCondition,
  winSolution,

  -- ** Launch parameters
  LaunchParams,
  ValidatedLaunchParams,

  -- *** Subrecord accessors
  temporal,
  robotNaming,
  recipesInfo,
  messageInfo,
  gameControls,
  randomness,
  discovery,
  landscape,
  robotInfo,
  pathCaching,

  -- ** GameState initialization
  initGameState,
  scenarioToGameState,
  CodeToRun (..),
  Sha1 (..),
  SolutionSource (..),
  parseCodeFile,

  -- * Utilities
  robotsAtLocation,
  robotsInArea,
  baseRobot,
  messageNotifications,
  currentScenarioPath,
  needsRedraw,
  replWorking,
  recalcViewCenterAndRedraw,
  viewingRegion,
  focusedRobot,
  RobotRange (..),
  focusedRange,
  getRadioRange,
  clearFocusedRobotLogUpdated,
  emitMessage,
  messageIsRecent,
  messageIsFromNearby,
  getRunCodePath,
  buildWorldTuples,
  genMultiWorld,
  genRobotTemplates,
  entityAt,
  zoomWorld,
  zoomRobots,
  SubworldDescription,
) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow ((&&&)))
import Control.Carrier.State.Lazy qualified as Fused
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Effect.State (State)
import Control.Effect.Throw
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad (forM, join)
import Data.Array (Array, listArray)
import Data.Bifunctor (first)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Foldable (toList)
import Data.Foldable.Extra (allM)
import Data.Function (on)
import Data.Int (Int32)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T (drop, take)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Linear (V2 (..))
import Swarm.Game.CESK (addTicks, emptyStore, finalValue, initMachine)
import Swarm.Game.Entity
import Swarm.Game.Failure (SystemFailure (..))
import Swarm.Game.Location
import Swarm.Game.Recipe (
  catRecipeMap,
  inRecipeMap,
  outRecipeMap,
 )
import Swarm.Game.Robot
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute

import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.ScenarioInfo
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step.Path.Type
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.Universe as U
import Swarm.Game.World (Coords (..), WorldFun (..), locToCoords, worldFunFromArray)
import Swarm.Game.World qualified as W
import Swarm.Game.World.Eval (runWorld)
import Swarm.Game.World.Gen (findGoodOrigin)
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Module (Module (Module))
import Swarm.Language.Pipeline (ProcessedTerm (ProcessedTerm), processTermEither)
import Swarm.Language.Syntax (SrcLoc (..), Syntax' (..), allConst)
import Swarm.Language.Typed (Typed (Typed))
import Swarm.Language.Types
import Swarm.Log
import Swarm.Util (applyWhen, binTuples, uniq, (?))
import Swarm.Util.Erasable
import Swarm.Util.Lens (makeLensesNoSigs)
import System.Clock qualified as Clock
import System.Random (mkStdGen, randomRIO)

newtype Sha1 = Sha1 String

data SolutionSource
  = ScenarioSuggested
  | -- | Includes the SHA1 of the program text
    -- for the purpose of corroborating solutions
    -- on a leaderboard.
    PlayerAuthored FilePath Sha1

data CodeToRun = CodeToRun SolutionSource ProcessedTerm

getRunCodePath :: CodeToRun -> Maybe FilePath
getRunCodePath (CodeToRun solutionSource _) = case solutionSource of
  ScenarioSuggested -> Nothing
  PlayerAuthored fp _ -> Just fp

parseCodeFile ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  m CodeToRun
parseCodeFile filepath = do
  contents <- sendIO $ TIO.readFile filepath
  pt@(ProcessedTerm (Module (Syntax' srcLoc _ _) _) _ _) <-
    either (throwError . CustomFailure) return (processTermEither contents)
  let strippedText = stripSrc srcLoc contents
      programBytestring = TL.encodeUtf8 $ TL.fromStrict strippedText
      sha1Hash = showDigest $ sha1 programBytestring
  return $ CodeToRun (PlayerAuthored filepath $ Sha1 sha1Hash) pt
 where
  stripSrc :: SrcLoc -> Text -> Text
  stripSrc (SrcLoc start end) txt = T.drop start $ T.take end txt
  stripSrc NoLoc txt = txt

------------------------------------------------------------
-- The main GameState record type
------------------------------------------------------------

-- | The main record holding the state for the game itself (as
--   distinct from the UI).  See the lenses below for access to its
--   fields.
data GameState = GameState
  { _creativeMode :: Bool
  , _temporal :: TemporalState
  , _winCondition :: WinCondition
  , _winSolution :: Maybe ProcessedTerm
  , _robotInfo :: Robots
  , _pathCaching :: PathCaching
  , _discovery :: Discovery
  , _randomness :: Randomness
  , _recipesInfo :: Recipes
  , _currentScenarioPath :: Maybe FilePath
  , _landscape :: Landscape
  , _needsRedraw :: Bool
  , _gameControls :: GameControls
  , _messageInfo :: Messages
  }

makeLensesNoSigs ''GameState

------------------------------------------------------------
-- Lenses
------------------------------------------------------------

-- | Is the user in creative mode (i.e. able to do anything without restriction)?
creativeMode :: Lens' GameState Bool

-- | Aspects of the temporal state of the game
temporal :: Lens' GameState TemporalState

-- | How to determine whether the player has won.
winCondition :: Lens' GameState WinCondition

-- | How to win (if possible). This is useful for automated testing
--   and to show help to cheaters (or testers).
winSolution :: Lens' GameState (Maybe ProcessedTerm)

-- | Get a list of all the robots at a particular location.
robotsAtLocation :: Cosmic Location -> GameState -> [Robot]
robotsAtLocation loc gs =
  mapMaybe (`IM.lookup` (gs ^. robotInfo . robotMap))
    . maybe [] IS.toList
    . M.lookup (loc ^. planar)
    . M.findWithDefault mempty (loc ^. subworld)
    . view (robotInfo . robotsByLocation)
    $ gs

-- | Registry for caching output of the @path@ command
pathCaching :: Lens' GameState PathCaching

-- | Get all the robots within a given Manhattan distance from a
--   location.
robotsInArea :: Cosmic Location -> Int32 -> Robots -> [Robot]
robotsInArea (Cosmic subworldName o) d rs = map (rm IM.!) rids
 where
  rm = rs ^. robotMap
  rl = rs ^. robotsByLocation
  rids =
    concatMap IS.elems $
      getElemsInArea o d $
        M.findWithDefault mempty subworldName rl

-- | The base robot, if it exists.
baseRobot :: Traversal' GameState Robot
baseRobot = robotInfo . robotMap . ix 0

-- | Inputs for randomness
randomness :: Lens' GameState Randomness

-- | Discovery state of entities, commands, recipes
discovery :: Lens' GameState Discovery

-- | Collection of recipe info
recipesInfo :: Lens' GameState Recipes

-- | The filepath of the currently running scenario.
--
-- This is useful as an index to the scenarios collection,
-- see 'Swarm.Game.ScenarioInfo.scenarioItemByPath'.
currentScenarioPath :: Lens' GameState (Maybe FilePath)

-- | Info about the lay of the land
landscape :: Lens' GameState Landscape

-- | Info about robots
robotInfo :: Lens' GameState Robots

-- | Whether the world view needs to be redrawn.
needsRedraw :: Lens' GameState Bool

-- | Controls, including REPL and key mapping
gameControls :: Lens' GameState GameControls

-- | Message info
messageInfo :: Lens' GameState Messages

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | Get the notification list of messages from the point of view of focused robot.
messageNotifications :: Getter GameState (Notifications LogEntry)
messageNotifications = to getNotif
 where
  getNotif gs = Notifications {_notificationsCount = length new, _notificationsContent = allUniq}
   where
    allUniq = uniq $ toList allMessages
    new = takeWhile (\l -> l ^. leTime > gs ^. messageInfo . lastSeenMessageTime) $ reverse allUniq
    -- creative players and system robots just see all messages (and focused robots logs)
    unchecked = gs ^. creativeMode || fromMaybe False (focusedRobot gs ^? _Just . systemRobot)
    messages = (if unchecked then id else focusedOrLatestClose) (gs ^. messageInfo . messageQueue)
    allMessages = Seq.sort $ focusedLogs <> messages
    focusedLogs = maybe Empty (view robotLog) (focusedRobot gs)
    -- classic players only get to see messages that they said and a one message that they just heard
    -- other they have to get from log
    latestMsg = messageIsRecent gs
    closeMsg = messageIsFromNearby (gs ^. robotInfo . viewCenter)
    generatedBy rid logEntry = case logEntry ^. leSource of
      RobotLog _ rid' _ -> rid == rid'
      _ -> False

    focusedOrLatestClose mq =
      (Seq.take 1 . Seq.reverse . Seq.filter closeMsg $ Seq.takeWhileR latestMsg mq)
        <> Seq.filter (generatedBy (gs ^. robotInfo . focusedRobotID)) mq

messageIsRecent :: GameState -> LogEntry -> Bool
messageIsRecent gs e = addTicks 1 (e ^. leTime) >= gs ^. temporal . ticks

-- | Reconciles the possibilities of log messages being
--   omnipresent and robots being in different worlds
messageIsFromNearby :: Cosmic Location -> LogEntry -> Bool
messageIsFromNearby l e = case e ^. leSource of
  SystemLog -> True
  RobotLog _ _ loc -> f loc
 where
  f logLoc = case cosmoMeasure manhattan l logLoc of
    InfinitelyFar -> False
    Measurable x -> x <= hearingDistance

-- | Recalculate the view center (and cache the result in the
--   'viewCenter' field) based on the current 'viewCenterRule'.  If
--   the 'viewCenterRule' specifies a robot which does not exist,
--   simply leave the current 'viewCenter' as it is. Set 'needsRedraw'
--   if the view center changes.
recalcViewCenterAndRedraw :: GameState -> GameState
recalcViewCenterAndRedraw g =
  g
    & robotInfo .~ newRobotInfo
    & (if ((/=) `on` (^. viewCenter)) oldRobotInfo newRobotInfo then needsRedraw .~ True else id)
 where
  oldRobotInfo = g ^. robotInfo
  newRobotInfo = recalcViewCenter oldRobotInfo

-- | Given a width and height, compute the region, centered on the
--   'viewCenter', that should currently be in view.
viewingRegion :: Cosmic Location -> (Int32, Int32) -> Cosmic W.BoundsRectangle
viewingRegion (Cosmic sw (Location cx cy)) (w, h) =
  Cosmic sw (W.Coords (rmin, cmin), W.Coords (rmax, cmax))
 where
  (rmin, rmax) = over both (+ (-cy - h `div` 2)) (0, h - 1)
  (cmin, cmax) = over both (+ (cx - w `div` 2)) (0, w - 1)

-- | Find out which robot has been last specified by the
--   'viewCenterRule', if any.
focusedRobot :: GameState -> Maybe Robot
focusedRobot g = g ^. robotInfo . robotMap . at (g ^. robotInfo . focusedRobotID)

-- | Type for describing how far away a robot is from the base, which
--   determines what kind of communication can take place.
data RobotRange
  = -- | Close; communication is perfect.
    Close
  | -- | Mid-range; communication is possible but lossy.
    MidRange Double
  | -- | Far; communication is not possible.
    Far
  deriving (Eq, Ord)

-- | Check how far away the focused robot is from the base.  @Nothing@
--   is returned if there is no focused robot; otherwise, return a
--   'RobotRange' value as follows.
--
--   * If we are in creative or scroll-enabled mode, the focused robot is
--   always considered 'Close'.
--   * Otherwise, there is a "minimum radius" and "maximum radius".
--
--       * If the robot is within the minimum radius, it is 'Close'.
--       * If the robot is between the minimum and maximum radii, it
--         is 'MidRange', with a 'Double' value ranging linearly from
--         0 to 1 proportional to the distance from the minimum to
--         maximum radius.  For example, @MidRange 0.5@ would indicate
--         a robot exactly halfway between the minimum and maximum
--         radii.
--       * If the robot is beyond the maximum radius, it is 'Far'.
--
--   * By default, the minimum radius is 16, and maximum is 64.
--   * Device augmentations
--
--       * If the focused robot has an @antenna@ installed, it doubles
--         both radii.
--       * If the base has an @antenna@ installed, it also doubles both radii.
focusedRange :: GameState -> Maybe RobotRange
focusedRange g = checkRange <$ maybeFocusedRobot
 where
  maybeBaseRobot = g ^. robotInfo . robotMap . at 0
  maybeFocusedRobot = focusedRobot g

  checkRange = case r of
    InfinitelyFar -> Far
    Measurable r' -> computedRange r'

  computedRange r'
    | g ^. creativeMode || g ^. landscape . worldScrollable || r' <= minRadius = Close
    | r' > maxRadius = Far
    | otherwise = MidRange $ (r' - minRadius) / (maxRadius - minRadius)

  -- Euclidean distance from the base to the view center.
  r = case maybeBaseRobot of
    -- if the base doesn't exist, we have bigger problems
    Nothing -> InfinitelyFar
    Just br -> cosmoMeasure euclidean (g ^. robotInfo . viewCenter) (br ^. robotLocation)

  (minRadius, maxRadius) = getRadioRange maybeBaseRobot maybeFocusedRobot

-- | Get the min/max communication radii given possible augmentations on each end
getRadioRange :: Maybe Robot -> Maybe Robot -> (Double, Double)
getRadioRange maybeBaseRobot maybeTargetRobot =
  (minRadius, maxRadius)
 where
  -- See whether the base or focused robot have antennas installed.
  baseInv, focInv :: Maybe Inventory
  baseInv = view equippedDevices <$> maybeBaseRobot
  focInv = view equippedDevices <$> maybeTargetRobot

  gain :: Maybe Inventory -> (Double -> Double)
  gain (Just inv)
    | countByName "antenna" inv > 0 = (* 2)
  gain _ = id

  -- Range radii.  Default thresholds are 16, 64; each antenna
  -- boosts the signal by 2x.
  minRadius, maxRadius :: Double
  (minRadius, maxRadius) = over both (gain baseInv . gain focInv) (16, 64)

-- | Clear the 'robotLogUpdated' flag of the focused robot.
clearFocusedRobotLogUpdated :: (Has (State Robots) sig m) => m ()
clearFocusedRobotLogUpdated = do
  n <- use focusedRobotID
  robotMap . ix n . robotLogUpdated .= False

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

-- | Add a message to the message queue.
emitMessage :: (Has (State GameState) sig m) => LogEntry -> m ()
emitMessage msg = messageInfo . messageQueue %= (|> msg) . dropLastIfLong
 where
  tooLong s = Seq.length s >= maxMessageQueueSize
  dropLastIfLong whole@(_oldest :<| newer) = if tooLong whole then newer else whole
  dropLastIfLong emptyQueue = emptyQueue

------------------------------------------------------------
-- Initialization
------------------------------------------------------------

type LaunchParams a = ParameterizableLaunchParams CodeToRun a

-- | In this stage in the UI pipeline, both fields
-- have already been validated, and "Nothing" means
-- that the field is simply absent.
type ValidatedLaunchParams = LaunchParams Identity

-- | Create an initial, fresh game state record when starting a new scenario.
initGameState :: GameStateConfig -> GameState
initGameState gsc =
  GameState
    { _creativeMode = False
    , _temporal = initTemporalState
    , _winCondition = NoWinCondition
    , _winSolution = Nothing
    , _robotInfo = initRobots gsc
    , _pathCaching = emptyPathCache
    , _discovery = initDiscovery
    , _randomness = initRandomness
    , _recipesInfo = initRecipeMaps gsc
    , _currentScenarioPath = Nothing
    , _landscape = initLandscape gsc
    , _needsRedraw = False
    , _gameControls = initGameControls
    , _messageInfo = initMessages
    }

type SubworldDescription = (SubworldName, ([IndexedTRobot], Seed -> WorldFun Int Entity))

buildWorldTuples :: Scenario -> NonEmpty SubworldDescription
buildWorldTuples s =
  NE.map (worldName &&& buildWorld) $
    s ^. scenarioWorlds

genMultiWorld :: NonEmpty SubworldDescription -> Seed -> W.MultiWorld Int Entity
genMultiWorld worldTuples s =
  M.map genWorld
    . M.fromList
    . NE.toList
    $ worldTuples
 where
  genWorld x = W.newWorld $ snd x s

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
genRobotTemplates :: Scenario -> NonEmpty (a, ([(Int, TRobot)], b)) -> [TRobot]
genRobotTemplates scenario worldTuples =
  locatedRobots ++ map snd (sortOn fst genRobots)
 where
  -- Keep only robots from the robot list with a concrete location;
  -- the others existed only to serve as a template for robots drawn
  -- in the world map
  locatedRobots = filter (isJust . view trobotLocation) $ scenario ^. scenarioRobots

  -- Subworld order as encountered in the scenario YAML file is preserved for
  -- the purpose of numbering robots, other than the "root" subworld
  -- guaranteed to be first.
  genRobots :: [(Int, TRobot)]
  genRobots = concat $ NE.toList $ NE.map (fst . snd) worldTuples

-- | Get the entity (if any) at a given location.
entityAt :: (Has (State GameState) sig m) => Cosmic Location -> m (Maybe Entity)
entityAt (Cosmic subworldName loc) =
  join <$> zoomWorld subworldName (W.lookupEntityM @Int (W.locToCoords loc))

-- | Perform an action requiring a 'Robots' state component in a
--   larger context with a 'GameState'.
zoomRobots ::
  (Has (State GameState) sig m) =>
  Fused.StateC Robots Identity b ->
  m b
zoomRobots n = do
  ri <- use robotInfo
  do
    let (ri', a) = run $ Fused.runState ri n
    robotInfo .= ri'
    return a

-- | Perform an action requiring a 'W.World' state component in a
--   larger context with a 'GameState'.
zoomWorld ::
  (Has (State GameState) sig m) =>
  SubworldName ->
  Fused.StateC (W.World Int Entity) Identity b ->
  m (Maybe b)
zoomWorld swName n = do
  mw <- use $ landscape . multiWorld
  forM (M.lookup swName mw) $ \w -> do
    let (w', a) = run (Fused.runState w n)
    landscape . multiWorld %= M.insert swName w'
    return a

-- | Matches definitions against the placements.
-- Fails fast (short-circuits) if a non-matching
-- cell is encountered.
ensureStructureIntact ::
  (Has (State GameState) sig m) =>
  FoundStructure ->
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

mkRecognizer ::
  (Has (State GameState) sig m) =>
  StaticStructureInfo ->
  m StructureRecognizer
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
  mkLogEntry (x, isIntact) =
    IntactPlacementLog
      isIntact
      ((Structure.name . originalDefinition . structureWithGrid) x)
      (upperLeftCorner x)

buildTagMap :: EntityMap -> Map Text (NonEmpty EntityName)
buildTagMap em =
  binTuples expanded
 where
  expanded = concatMap (\(k, vs) -> [(v, k) | v <- S.toList vs]) $ M.toList tagsByEntity
  tagsByEntity = M.map (view entityTags) $ entitiesByName em

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
  recognizer =
    runIdentity $
      Fused.evalState preliminaryGameState $
        mkRecognizer (scenario ^. scenarioStructures)

  gs = initGameState gsc
  preliminaryGameState =
    gs
      & robotInfo %~ setRobotInfo baseID robotList'
      & creativeMode .~ scenario ^. scenarioCreative
      & winCondition .~ theWinCondition
      & winSolution .~ scenario ^. scenarioSolution
      & discovery . availableCommands .~ Notifications 0 initialCommands
      & discovery . knownEntities .~ scenario ^. scenarioKnown
      & discovery . tagMembers .~ buildTagMap em
      & randomness . seed .~ theSeed
      & randomness . randGen .~ mkStdGen theSeed
      & recipesInfo %~ modifyRecipesInfo
      & landscape . entityMap .~ em
      & landscape . worldNavigation .~ scenario ^. scenarioNavigation
      & landscape . multiWorld .~ genMultiWorld worldTuples theSeed
      -- TODO (#1370): Should we allow subworlds to have their own scrollability?
      -- Leaning toward no , but for now just adopt the root world scrollability
      -- as being universal.
      & landscape . worldScrollable .~ NE.head (scenario ^. scenarioWorlds) ^. to scrollable
      & gameControls . initiallyRunCode .~ initialCodeToRun
      & gameControls . replStatus .~ case running of -- When the base starts out running a program, the REPL status must be set to working,
      -- otherwise the store of definition cells is not saved (see #333, #838)
        False -> REPLDone Nothing
        True -> REPLWorking (Typed Nothing PolyUnit mempty)
      & temporal . robotStepsPerTick .~ ((scenario ^. scenarioStepsPerTick) ? defaultRobotStepsPerTick)

  robotList' = (robotCreatedAt .~ now) <$> robotList

  modifyRecipesInfo oldRecipesInfo =
    oldRecipesInfo
      & recipesOut %~ addRecipesWith outRecipeMap
      & recipesIn %~ addRecipesWith inRecipeMap
      & recipesCat %~ addRecipesWith catRecipeMap

  em = initEntities gsc <> scenario ^. scenarioEntities
  baseID = 0
  (things, devices) = partition (null . view entityCapabilities) (M.elems (entitiesByName em))

  getCodeToRun (CodeToRun _ s) = s

  robotsByBasePrecedence = genRobotTemplates scenario worldTuples

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
          Just pt -> const $ initMachine pt Ctx.empty emptyStore
      -- If we are in creative mode, give base all the things
      & ix baseID
        . robotInventory
        %~ case scenario ^. scenarioCreative of
          False -> id
          True -> union (fromElems (map (0,) things))
      & ix baseID
        . equippedDevices
        %~ case scenario ^. scenarioCreative of
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
  initialCaps = mconcat $ map allCapabilities robotList
  initialCommands =
    filter
      (maybe True (`S.member` initialCaps) . constCaps)
      allConst

  worldTuples = buildWorldTuples scenario

  theWinCondition =
    maybe
      NoWinCondition
      (\x -> WinConditions Ongoing (ObjectiveCompletion (CompletionBuckets (NE.toList x) mempty mempty) mempty))
      (NE.nonEmpty (scenario ^. scenarioObjectives))

  addRecipesWith f = IM.unionWith (<>) (f $ scenario ^. scenarioRecipes)

-- | Create an initial game state corresponding to the given scenario.
scenarioToGameState ::
  Scenario ->
  ValidatedLaunchParams ->
  GameStateConfig ->
  IO GameState
scenarioToGameState scenario (LaunchParams (Identity userSeed) (Identity toRun)) gsc = do
  -- Decide on a seed.  In order of preference, we will use:
  --   1. seed value provided by the user
  --   2. seed value specified in the scenario description
  --   3. randomly chosen seed value
  theSeed <- case userSeed <|> scenario ^. scenarioSeed of
    Just s -> return s
    Nothing -> randomRIO (0, maxBound :: Int)

  now <- Clock.getTime Clock.Monotonic
  return $ pureScenarioToGameState scenario theSeed now toRun gsc

-- | Take a world description, parsed from a scenario file, and turn
--   it into a list of located robots and a world function.
buildWorld :: WorldDescription -> ([IndexedTRobot], Seed -> WorldFun Int Entity)
buildWorld WorldDescription {..} = (robots worldName, first fromEnum . wf)
 where
  rs = fromIntegral $ length area
  cs = fromIntegral $ maybe 0 length $ listToMaybe area
  Coords (ulr, ulc) = locToCoords ul

  worldGrid :: [[(TerrainType, Erasable Entity)]]
  worldGrid = (map . map) (cellTerrain &&& cellEntity) area

  worldArray :: Array (Int32, Int32) (TerrainType, Erasable Entity)
  worldArray = listArray ((ulr, ulc), (ulr + rs - 1, ulc + cs - 1)) (concat worldGrid)

  dslWF, arrayWF :: Seed -> WorldFun TerrainType Entity
  dslWF = maybe mempty ((applyWhen offsetOrigin findGoodOrigin .) . runWorld) worldProg
  arrayWF = const (worldFunFromArray worldArray)

  wf = dslWF <> arrayWF

  -- Get all the robots described in cells and set their locations appropriately
  robots :: SubworldName -> [IndexedTRobot]
  robots swName =
    area
      & traversed Control.Lens.<.> traversed %@~ (,) -- add (r,c) indices
      & concat
      & concatMap
        ( \((fromIntegral -> r, fromIntegral -> c), Cell _ _ robotList) ->
            let robotWithLoc = trobotLocation ?~ Cosmic swName (W.coordsToLoc (Coords (ulr + r, ulc + c)))
             in map (fmap robotWithLoc) robotList
        )
