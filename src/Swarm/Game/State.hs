{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
module Swarm.Game.State (
  -- * Game state record and related types
  ViewCenterRule (..),
  REPLStatus (..),
  WinStatus (..),
  WinCondition (..),
  ObjectiveCompletion (..),
  _NoWinCondition,
  _WinConditions,
  Announcement (..),
  RunStatus (..),
  Seed,
  Step (..),
  SingleStep (..),
  GameState,

  -- ** GameState fields
  creativeMode,
  winCondition,
  winSolution,
  robotMap,
  robotsByLocation,
  robotsAtLocation,
  robotsWatching,
  robotsInArea,
  baseRobot,
  activeRobots,
  waitingRobots,
  messageNotifications,
  seed,
  randGen,
  currentScenarioPath,
  viewCenterRule,
  viewCenter,
  needsRedraw,
  focusedRobotID,

  -- *** Subrecord accessors
  temporal,
  robotNaming,
  recipesInfo,
  messageInfo,
  gameControls,
  discovery,
  landscape,

  -- ** GameState subrecords

  -- *** Temporal state
  TemporalState,
  gameStep,
  runStatus,
  ticks,
  robotStepsPerTick,
  paused,

  -- *** Robot naming
  RobotNaming,
  nameGenerator,
  gensym,

  -- *** Recipes
  Recipes,
  recipesOut,
  recipesIn,
  recipesCat,

  -- *** Messages
  Messages,
  messageQueue,
  lastSeenMessageTime,
  announcementQueue,

  -- *** Controls
  GameControls,
  initiallyRunCode,
  replStatus,
  replNextValueIndex,
  replWorking,
  replActiveType,
  inputHandler,

  -- *** Discovery
  Discovery,
  allDiscoveredEntities,
  availableRecipes,
  availableCommands,
  knownEntities,
  gameAchievements,
  structureRecognition,

  -- *** Landscape
  Landscape,
  worldNavigation,
  multiWorld,
  worldScrollable,
  entityMap,

  -- ** Notifications
  Notifications (..),
  notificationsCount,
  notificationsContent,

  -- ** Launch parameters
  LaunchParams,
  ValidatedLaunchParams,

  -- ** GameState initialization
  GameStateConfig (..),
  initGameState,
  scenarioToGameState,
  CodeToRun (..),
  Sha1 (..),
  SolutionSource (..),
  parseCodeFile,

  -- * Utilities
  applyViewCenterRule,
  recalcViewCenter,
  modifyViewCenter,
  viewingRegion,
  unfocus,
  focusedRobot,
  RobotRange (..),
  focusedRange,
  getRadioRange,
  clearFocusedRobotLogUpdated,
  addRobot,
  addRobotToLocation,
  addTRobot,
  emitMessage,
  wakeWatchingRobots,
  sleepUntil,
  sleepForever,
  wakeUpRobotsDoneSleeping,
  deleteRobot,
  removeRobotFromLocationMap,
  activateRobot,
  toggleRunStatus,
  messageIsRecent,
  messageIsFromNearby,
  getRunCodePath,
  buildWorldTuples,
  genMultiWorld,
  genRobotTemplates,
  entityAt,
  zoomWorld,
) where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow ((&&&)))
import Control.Carrier.State.Lazy qualified as Fused
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Effect.State (State)
import Control.Effect.Throw
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad (forM, forM_, join)
import Data.Aeson (FromJSON, ToJSON)
import Data.Array (Array, listArray)
import Data.Bifunctor (first)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Foldable (toList)
import Data.Foldable.Extra (allM)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.IntSet.Lens (setOf)
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
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.CESK (CESK (Waiting), TickNumber (..), addTicks, emptyStore, finalValue, initMachine)
import Swarm.Game.Entity
import Swarm.Game.Failure (SystemFailure (..))
import Swarm.Game.Location
import Swarm.Game.Recipe (
  Recipe,
  catRecipeMap,
  inRecipeMap,
  outRecipeMap,
 )
import Swarm.Game.ResourceLoading (NameGenerator)
import Swarm.Game.Robot
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Navigation.Portal (Navigation (..))
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.ScenarioInfo
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.Universe as U
import Swarm.Game.World (Coords (..), WorldFun (..), locToCoords, worldFunFromArray)
import Swarm.Game.World qualified as W
import Swarm.Game.World.Eval (runWorld)
import Swarm.Game.World.Gen (Seed, findGoodOrigin)
import Swarm.Game.World.Typecheck (WorldMap)
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Module (Module (Module))
import Swarm.Language.Pipeline (ProcessedTerm (ProcessedTerm), processTermEither)
import Swarm.Language.Syntax (Const, SrcLoc (..), Syntax' (..), allConst)
import Swarm.Language.Typed (Typed (Typed))
import Swarm.Language.Types
import Swarm.Language.Value (Value)
import Swarm.Log
import Swarm.Util (applyWhen, binTuples, surfaceEmpty, uniq, (<+=), (<<.=), (?))
import Swarm.Util.Erasable
import Swarm.Util.Lens (makeLensesExcluding, makeLensesNoSigs)
import System.Clock qualified as Clock
import System.Random (StdGen, mkStdGen, randomRIO)

------------------------------------------------------------
-- Subsidiary data types
------------------------------------------------------------

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = -- | The view should be centered on an absolute position.
    VCLocation (Cosmic Location)
  | -- | The view should be centered on a certain robot.
    VCRobot RID
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makePrisms ''ViewCenterRule

-- | A data type to represent the current status of the REPL.
data REPLStatus
  = -- | The REPL is not doing anything actively at the moment.
    --   We persist the last value and its type though.
    --
    --   INVARIANT: the 'Value' stored here is not a 'Swarm.Language.Value.VResult'.
    REPLDone (Maybe (Typed Value))
  | -- | A command entered at the REPL is currently being run.  The
    --   'Polytype' represents the type of the expression that was
    --   entered.  The @Maybe Value@ starts out as 'Nothing' and gets
    --   filled in with a result once the command completes.
    REPLWorking (Typed (Maybe Value))
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data WinStatus
  = -- | There are one or more objectives remaining that the player
    -- has not yet accomplished.
    Ongoing
  | -- | The player has won.
    -- The boolean indicates whether they have
    -- already been congratulated.
    Won Bool
  | -- | The player has completed certain "goals" that preclude
    -- (via negative prerequisites) the completion of all of the
    -- required goals.
    -- The boolean indicates whether they have
    -- already been informed.
    Unwinnable Bool
  deriving (Show, Generic, FromJSON, ToJSON)

data WinCondition
  = -- | There is no winning condition.
    NoWinCondition
  | -- | NOTE: It is possible to continue to achieve "optional" objectives
    -- even after the game has been won (or deemed unwinnable).
    WinConditions WinStatus ObjectiveCompletion
  deriving (Show, Generic, FromJSON, ToJSON)

makePrisms ''WinCondition

instance ToSample WinCondition where
  toSamples _ =
    SD.samples
      [ NoWinCondition
      -- TODO: #1552 add simple objective sample
      ]

-- | A data type to keep track of the pause mode.
data RunStatus
  = -- | The game is running.
    Running
  | -- | The user paused the game, and it should stay pause after visiting the help.
    ManualPause
  | -- | The game got paused while visiting the help,
    --   and it should unpause after returning back to the game.
    AutoPause
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Switch (auto or manually) paused game to running and running to manually paused.
--
--   Note that this function is not safe to use in the app directly, because the UI
--   also tracks time between ticks---use 'Swarm.TUI.Controller.safeTogglePause' instead.
toggleRunStatus :: RunStatus -> RunStatus
toggleRunStatus s = if s == Running then ManualPause else Running

-- | A data type to keep track of some kind of log or sequence, with
--   an index to remember which ones are "new" and which ones have
--   "already been seen".
data Notifications a = Notifications
  { _notificationsCount :: Int
  , _notificationsContent :: [a]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Semigroup (Notifications a) where
  Notifications count1 xs1 <> Notifications count2 xs2 = Notifications (count1 + count2) (xs1 <> xs2)

instance Monoid (Notifications a) where
  mempty = Notifications 0 []

makeLenses ''Notifications

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

-- | By default, robots may make a maximum of 100 CESK machine steps
--   during one game tick.
defaultRobotStepsPerTick :: Int
defaultRobotStepsPerTick = 100

-- | Type for remembering which robots will be run next in a robot step mode.
--
-- Once some robots have run, we need to store 'RID' to know which ones should go next.
-- At 'SBefore' no robots were run yet, so it is safe to transition to and from 'WorldTick'.
--
-- @
--                     tick
--     ┌────────────────────────────────────┐
--     │                                    │
--     │               step                 │
--     │              ┌────┐                │
--     ▼              ▼    │                │
-- ┌───────┐ step  ┌───────┴───┐ step  ┌────┴─────┐
-- │SBefore├──────►│SSingle RID├──────►│SAfter RID│
-- └──┬────┘       └───────────┘       └────┬─────┘
--    │ ▲ player        ▲                   │
--    ▼ │ switch        └───────────────────┘
-- ┌────┴────┐             view RID > oldRID
-- │WorldTick│
-- └─────────┘
-- @
data SingleStep
  = -- | Run the robots from the beginning until the focused robot (noninclusive).
    SBefore
  | -- | Run a single step of the focused robot.
    SSingle RID
  | -- | Run robots after the (previously) focused robot and finish the tick.
    SAfter RID

-- | Game step mode - we use the single step mode when debugging robot 'CESK' machine.
data Step = WorldTick | RobotStep SingleStep

data Recipes = Recipes
  { _recipesOut :: IntMap [Recipe Entity]
  , _recipesIn :: IntMap [Recipe Entity]
  , _recipesCat :: IntMap [Recipe Entity]
  }

makeLensesNoSigs ''Recipes

-- | All recipes the game knows about, indexed by outputs.
recipesOut :: Lens' Recipes (IntMap [Recipe Entity])

-- | All recipes the game knows about, indexed by inputs.
recipesIn :: Lens' Recipes (IntMap [Recipe Entity])

-- | All recipes the game knows about, indexed by requirement/catalyst.
recipesCat :: Lens' Recipes (IntMap [Recipe Entity])

data Messages = Messages
  { _messageQueue :: Seq LogEntry
  , _lastSeenMessageTime :: TickNumber
  , _announcementQueue :: Seq Announcement
  }

makeLensesNoSigs ''Messages

-- | A queue of global messages.
--
-- Note that we put the newest entry to the right.
messageQueue :: Lens' Messages (Seq LogEntry)

-- | Last time message queue has been viewed (used for notification).
lastSeenMessageTime :: Lens' Messages TickNumber

-- | A queue of global announcements.
-- Note that this is distinct from the 'messageQueue',
-- which is for messages emitted by robots.
--
-- Note that we put the newest entry to the right.
announcementQueue :: Lens' Messages (Seq Announcement)

data RobotNaming = RobotNaming
  { _nameGenerator :: NameGenerator
  , _gensym :: Int
  }

makeLensesExcluding ['_nameGenerator] ''RobotNaming

--- | Read-only list of words, for use in building random robot names.
nameGenerator :: Getter RobotNaming NameGenerator
nameGenerator = to _nameGenerator

-- | A counter used to generate globally unique IDs.
gensym :: Lens' RobotNaming Int

data TemporalState = TemporalState
  { _gameStep :: Step
  , _runStatus :: RunStatus
  , _ticks :: TickNumber
  , _robotStepsPerTick :: Int
  }

makeLensesNoSigs ''TemporalState

-- | How to step the game: 'WorldTick' or 'RobotStep' for debugging the 'CESK' machine.
gameStep :: Lens' TemporalState Step

-- | The current 'RunStatus'.
runStatus :: Lens' TemporalState RunStatus

-- | Whether the game is currently paused.
paused :: Getter TemporalState Bool
paused = to (\s -> s ^. runStatus /= Running)

-- | The number of ticks elapsed since the game started.
ticks :: Lens' TemporalState TickNumber

-- | The maximum number of CESK machine steps a robot may take during
--   a single tick.
robotStepsPerTick :: Lens' TemporalState Int

data GameControls = GameControls
  { _replStatus :: REPLStatus
  , _replNextValueIndex :: Integer
  , _inputHandler :: Maybe (Text, Value)
  , _initiallyRunCode :: Maybe ProcessedTerm
  }

makeLensesNoSigs ''GameControls

-- | The current status of the REPL.
replStatus :: Lens' GameControls REPLStatus

-- | The index of the next @it{index}@ value
replNextValueIndex :: Lens' GameControls Integer

-- | The currently installed input handler and hint text.
inputHandler :: Lens' GameControls (Maybe (Text, Value))

-- | Code that is run upon scenario start, before any
-- REPL interaction.
initiallyRunCode :: Lens' GameControls (Maybe ProcessedTerm)

data Discovery = Discovery
  { _allDiscoveredEntities :: Inventory
  , _availableRecipes :: Notifications (Recipe Entity)
  , _availableCommands :: Notifications Const
  , _knownEntities :: [Text]
  , _gameAchievements :: Map GameplayAchievement Attainment
  , _structureRecognition :: StructureRecognizer
  }

makeLensesNoSigs ''Discovery

-- | The list of entities that have been discovered.
allDiscoveredEntities :: Lens' Discovery Inventory

-- | The list of available recipes.
availableRecipes :: Lens' Discovery (Notifications (Recipe Entity))

-- | The list of available commands.
availableCommands :: Lens' Discovery (Notifications Const)

-- | The names of entities that should be considered \"known\", that is,
--   robots know what they are without having to scan them.
knownEntities :: Lens' Discovery [Text]

-- | Map of in-game achievements that were obtained
gameAchievements :: Lens' Discovery (Map GameplayAchievement Attainment)

-- | Recognizer for robot-constructed structures
structureRecognition :: Lens' Discovery StructureRecognizer

data Landscape = Landscape
  { _worldNavigation :: Navigation (M.Map SubworldName) Location
  , _multiWorld :: W.MultiWorld Int Entity
  , _entityMap :: EntityMap
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
multiWorld :: Lens' Landscape (W.MultiWorld Int Entity)

-- | The catalog of all entities that the game knows about.
entityMap :: Lens' Landscape EntityMap

-- | Whether the world map is supposed to be scrollable or not.
worldScrollable :: Lens' Landscape Bool

-- | The main record holding the state for the game itself (as
--   distinct from the UI).  See the lenses below for access to its
--   fields.
data GameState = GameState
  { _creativeMode :: Bool
  , _temporal :: TemporalState
  , _winCondition :: WinCondition
  , _winSolution :: Maybe ProcessedTerm
  , _robotMap :: IntMap Robot
  , -- A set of robots to consider for the next game tick. It is guaranteed to
    -- be a subset of the keys of 'robotMap'. It may contain waiting or idle
    -- robots. But robots that are present in 'robotMap' and not in 'activeRobots'
    -- are guaranteed to be either waiting or idle.
    _activeRobots :: IntSet
  , -- A set of probably waiting robots, indexed by probable wake-up time. It
    -- may contain robots that are in fact active or idle, as well as robots
    -- that do not exist anymore. Its only guarantee is that once a robot name
    -- with its wake up time is inserted in it, it will remain there until the
    -- wake-up time is reached, at which point it is removed via
    -- 'wakeUpRobotsDoneSleeping'.
    -- Waiting robots for a given time are a list because it is cheaper to
    -- prepend to a list than insert into a 'Set'.
    _waitingRobots :: Map TickNumber [RID]
  , _robotsByLocation :: Map SubworldName (Map Location IntSet)
  , -- This member exists as an optimization so
    -- that we do not have to iterate over all "waiting" robots,
    -- since there may be many.
    _robotsWatching :: Map (Cosmic Location) (S.Set RID)
  , _discovery :: Discovery
  , _seed :: Seed
  , _randGen :: StdGen
  , _robotNaming :: RobotNaming
  , _recipesInfo :: Recipes
  , _currentScenarioPath :: Maybe FilePath
  , _landscape :: Landscape
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter :: Cosmic Location
  , _needsRedraw :: Bool
  , _gameControls :: GameControls
  , _messageInfo :: Messages
  , _focusedRobotID :: RID
  }

------------------------------------------------------------
-- Lenses
------------------------------------------------------------

-- We want to access active and waiting robots via lenses inside
-- this module but to expose it as a Getter to protect invariants.
makeLensesFor
  [ ("_activeRobots", "internalActiveRobots")
  , ("_waitingRobots", "internalWaitingRobots")
  ]
  ''GameState

makeLensesExcluding ['_viewCenter, '_focusedRobotID, '_viewCenterRule, '_activeRobots, '_waitingRobots] ''GameState

-- | Is the user in creative mode (i.e. able to do anything without restriction)?
creativeMode :: Lens' GameState Bool

-- | Aspects of the temporal state of the game
temporal :: Lens' GameState TemporalState

-- | How to determine whether the player has won.
winCondition :: Lens' GameState WinCondition

-- | How to win (if possible). This is useful for automated testing
--   and to show help to cheaters (or testers).
winSolution :: Lens' GameState (Maybe ProcessedTerm)

-- | All the robots that currently exist in the game, indexed by ID.
robotMap :: Lens' GameState (IntMap Robot)

-- | The names of all robots that currently exist in the game, indexed by
--   location (which we need both for /e.g./ the @salvage@ command as
--   well as for actually drawing the world).  Unfortunately there is
--   no good way to automatically keep this up to date, since we don't
--   just want to completely rebuild it every time the 'robotMap'
--   changes.  Instead, we just make sure to update it every time the
--   location of a robot changes, or a robot is created or destroyed.
--   Fortunately, there are relatively few ways for these things to
--   happen.
robotsByLocation :: Lens' GameState (Map SubworldName (Map Location IntSet))

-- | Get a list of all the robots at a particular location.
robotsAtLocation :: Cosmic Location -> GameState -> [Robot]
robotsAtLocation loc gs =
  mapMaybe (`IM.lookup` (gs ^. robotMap))
    . maybe [] IS.toList
    . M.lookup (loc ^. planar)
    . M.findWithDefault mempty (loc ^. subworld)
    . view robotsByLocation
    $ gs

-- | Get a list of all the robots that are \"watching\" by location.
robotsWatching :: Lens' GameState (Map (Cosmic Location) (S.Set RID))

-- | Get all the robots within a given Manhattan distance from a
--   location.
robotsInArea :: Cosmic Location -> Int32 -> GameState -> [Robot]
robotsInArea (Cosmic subworldName o) d gs = map (rm IM.!) rids
 where
  rm = gs ^. robotMap
  rl = gs ^. robotsByLocation
  rids =
    concatMap IS.elems $
      getElemsInArea o d $
        M.findWithDefault mempty subworldName rl

-- | The base robot, if it exists.
baseRobot :: Traversal' GameState Robot
baseRobot = robotMap . ix 0

-- | The names of the robots that are currently not sleeping.
activeRobots :: Getter GameState IntSet
activeRobots = internalActiveRobots

-- | The names of the robots that are currently sleeping, indexed by wake up
--   time. Note that this may not include all sleeping robots, particularly
--   those that are only taking a short nap (e.g. @wait 1@).
waitingRobots :: Getter GameState (Map TickNumber [RID])
waitingRobots = internalWaitingRobots

-- | Discovery state of entities, commands, recipes
discovery :: Lens' GameState Discovery

-- | The initial seed that was used for the random number generator,
--   and world generation.
seed :: Lens' GameState Seed

-- | Pseudorandom generator initialized at start.
randGen :: Lens' GameState StdGen

-- | State and data for assigning identifiers to robots
robotNaming :: Lens' GameState RobotNaming

-- | Collection of recipe info
recipesInfo :: Lens' GameState Recipes

-- | The filepath of the currently running scenario.
--
-- This is useful as an index to the scenarios collection,
-- see 'Swarm.Game.ScenarioInfo.scenarioItemByPath'.
currentScenarioPath :: Lens' GameState (Maybe FilePath)

-- | Info about the lay of the land
landscape :: Lens' GameState Landscape

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter GameState (Cosmic Location)
viewCenter = to _viewCenter

-- | Whether the world view needs to be redrawn.
needsRedraw :: Lens' GameState Bool

-- | Controls, including REPL and key mapping
gameControls :: Lens' GameState GameControls

-- | Message info
messageInfo :: Lens' GameState Messages

-- | The current robot in focus.
--
-- It is only a 'Getter' because this value should be updated only when
-- the 'viewCenterRule' is specified to be a robot.
--
-- Technically it's the last robot ID specified by 'viewCenterRule',
-- but that robot may not be alive anymore - to be safe use 'focusedRobot'.
focusedRobotID :: Getter GameState RID
focusedRobotID = to _focusedRobotID

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | The current rule for determining the center of the world view.
--   It updates also, 'viewCenter' and 'focusedRobot' to keep
--   everything synchronized.
viewCenterRule :: Lens' GameState ViewCenterRule
viewCenterRule = lens getter setter
 where
  getter :: GameState -> ViewCenterRule
  getter = _viewCenterRule

  -- The setter takes care of updating 'viewCenter' and 'focusedRobot'
  -- So non of this fields get out of sync.
  setter :: GameState -> ViewCenterRule -> GameState
  setter g rule =
    case rule of
      VCLocation loc -> g {_viewCenterRule = rule, _viewCenter = loc}
      VCRobot rid ->
        let robotcenter = g ^? robotMap . ix rid . robotLocation
         in -- retrieve the loc of the robot if it exists, Nothing otherwise.
            -- sometimes, lenses are amazing...
            case robotcenter of
              Nothing -> g
              Just loc -> g {_viewCenterRule = rule, _viewCenter = loc, _focusedRobotID = rid}

-- | Whether the repl is currently working.
replWorking :: Getter GameControls Bool
replWorking = to (\s -> matchesWorking $ s ^. replStatus)
 where
  matchesWorking (REPLDone _) = False
  matchesWorking (REPLWorking _) = True

-- | Either the type of the command being executed, or of the last command
replActiveType :: Getter REPLStatus (Maybe Polytype)
replActiveType = to getter
 where
  getter (REPLDone (Just (Typed _ typ _))) = Just typ
  getter (REPLWorking (Typed _ typ _)) = Just typ
  getter _ = Nothing

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
    closeMsg = messageIsFromNearby (gs ^. viewCenter)
    generatedBy rid logEntry = case logEntry ^. leSource of
      RobotLog _ rid' _ -> rid == rid'
      _ -> False

    focusedOrLatestClose mq =
      (Seq.take 1 . Seq.reverse . Seq.filter closeMsg $ Seq.takeWhileR latestMsg mq)
        <> Seq.filter (generatedBy (gs ^. focusedRobotID)) mq

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

-- | Given a current mapping from robot names to robots, apply a
--   'ViewCenterRule' to derive the location it refers to.  The result
--   is 'Maybe' because the rule may refer to a robot which does not
--   exist.
applyViewCenterRule :: ViewCenterRule -> IntMap Robot -> Maybe (Cosmic Location)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation

-- | Recalculate the view center (and cache the result in the
--   'viewCenter' field) based on the current 'viewCenterRule'.  If
--   the 'viewCenterRule' specifies a robot which does not exist,
--   simply leave the current 'viewCenter' as it is. Set 'needsRedraw'
--   if the view center changes.
recalcViewCenter :: GameState -> GameState
recalcViewCenter g =
  g
    { _viewCenter = newViewCenter
    }
    & (if newViewCenter /= oldViewCenter then needsRedraw .~ True else id)
 where
  oldViewCenter = g ^. viewCenter
  newViewCenter =
    fromMaybe oldViewCenter $
      applyViewCenterRule (g ^. viewCenterRule) (g ^. robotMap)

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter :: (Cosmic Location -> Cosmic Location) -> GameState -> GameState
modifyViewCenter update g =
  g
    & case g ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _ -> viewCenterRule .~ VCLocation (update (g ^. viewCenter))

-- | "Unfocus" by modifying the view center rule to look at the
--   current location instead of a specific robot, and also set the
--   focused robot ID to an invalid value.  In classic mode this
--   causes the map view to become nothing but static.
unfocus :: GameState -> GameState
unfocus = (\g -> g {_focusedRobotID = -1000}) . modifyViewCenter id

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
focusedRobot g = g ^. robotMap . at (g ^. focusedRobotID)

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
  maybeBaseRobot = g ^. robotMap . at 0
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
    Just br -> cosmoMeasure euclidean (g ^. viewCenter) (br ^. robotLocation)

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
clearFocusedRobotLogUpdated :: (Has (State GameState) sig m) => m ()
clearFocusedRobotLogUpdated = do
  n <- use focusedRobotID
  robotMap . ix n . robotLogUpdated .= False

-- | Add a concrete instance of a robot template to the game state:
--   First, generate a unique ID number for it.  Then, add it to the
--   main robot map, the active robot set, and to to the index of
--   robots by location. Return the updated robot.
addTRobot :: (Has (State GameState) sig m) => TRobot -> m Robot
addTRobot r = do
  rid <- robotNaming . gensym <+= 1
  let r' = instantiateRobot rid r
  addRobot r'
  return r'

-- | Add a robot to the game state, adding it to the main robot map,
--   the active robot set, and to to the index of robots by
--   location.
addRobot :: (Has (State GameState) sig m) => Robot -> m ()
addRobot r = do
  let rid = r ^. robotID

  robotMap %= IM.insert rid r
  addRobotToLocation rid $ r ^. robotLocation
  internalActiveRobots %= IS.insert rid

-- | Helper function for updating the "robotsByLocation" bookkeeping
addRobotToLocation :: (Has (State GameState) sig m) => RID -> Cosmic Location -> m ()
addRobotToLocation rid rLoc =
  robotsByLocation
    %= M.insertWith
      (M.unionWith IS.union)
      (rLoc ^. subworld)
      (M.singleton (rLoc ^. planar) (IS.singleton rid))

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

-- | Add a message to the message queue.
emitMessage :: (Has (State GameState) sig m) => LogEntry -> m ()
emitMessage msg = messageInfo . messageQueue %= (|> msg) . dropLastIfLong
 where
  tooLong s = Seq.length s >= maxMessageQueueSize
  dropLastIfLong whole@(_oldest :<| newer) = if tooLong whole then newer else whole
  dropLastIfLong emptyQueue = emptyQueue

-- | Takes a robot out of the 'activeRobots' set and puts it in the 'waitingRobots'
--   queue.
sleepUntil :: (Has (State GameState) sig m) => RID -> TickNumber -> m ()
sleepUntil rid time = do
  internalActiveRobots %= IS.delete rid
  internalWaitingRobots . at time . non [] %= (rid :)

-- | Takes a robot out of the 'activeRobots' set.
sleepForever :: (Has (State GameState) sig m) => RID -> m ()
sleepForever rid = internalActiveRobots %= IS.delete rid

-- | Adds a robot to the 'activeRobots' set.
activateRobot :: (Has (State GameState) sig m) => RID -> m ()
activateRobot rid = internalActiveRobots %= IS.insert rid

-- | Removes robots whose wake up time matches the current game ticks count
--   from the 'waitingRobots' queue and put them back in the 'activeRobots' set
--   if they still exist in the keys of 'robotMap'.
wakeUpRobotsDoneSleeping :: (Has (State GameState) sig m) => m ()
wakeUpRobotsDoneSleeping = do
  time <- use $ temporal . ticks
  mrids <- internalWaitingRobots . at time <<.= Nothing
  case mrids of
    Nothing -> return ()
    Just rids -> do
      robots <- use robotMap
      let aliveRids = filter (`IM.member` robots) rids
      internalActiveRobots %= IS.union (IS.fromList aliveRids)

      -- These robots' wake times may have been moved "forward"
      -- by 'wakeWatchingRobots'.
      clearWatchingRobots rids

-- | Clear the "watch" state of all of the
-- awakened robots
clearWatchingRobots ::
  (Has (State GameState) sig m) =>
  [RID] ->
  m ()
clearWatchingRobots rids = do
  robotsWatching %= M.map (`S.difference` S.fromList rids)

-- | Iterates through all of the currently @wait@-ing robots,
-- and moves forward the wake time of the ones that are @watch@-ing this location.
--
-- NOTE: Clearing 'TickNumber' map entries from 'internalWaitingRobots'
-- upon wakeup is handled by 'wakeUpRobotsDoneSleeping'
wakeWatchingRobots :: (Has (State GameState) sig m) => Cosmic Location -> m ()
wakeWatchingRobots loc = do
  currentTick <- use $ temporal . ticks
  waitingMap <- use waitingRobots
  rMap <- use robotMap
  watchingMap <- use robotsWatching

  -- The bookkeeping updates to robot waiting
  -- states are prepared in 4 steps...

  let -- Step 1: Identify the robots that are watching this location.
      botsWatchingThisLoc :: [Robot]
      botsWatchingThisLoc =
        mapMaybe (`IM.lookup` rMap) $
          S.toList $
            M.findWithDefault mempty loc watchingMap

      -- Step 2: Get the target wake time for each of these robots
      wakeTimes :: [(RID, TickNumber)]
      wakeTimes = mapMaybe (sequenceA . (view robotID &&& waitingUntil)) botsWatchingThisLoc

      wakeTimesToPurge :: Map TickNumber (S.Set RID)
      wakeTimesToPurge = M.fromListWith (<>) $ map (fmap S.singleton . swap) wakeTimes

      -- Step 3: Take these robots out of their time-indexed slot in "waitingRobots".
      -- To preserve performance, this should be done without iterating over the
      -- entire "waitingRobots" map.
      filteredWaiting = foldr f waitingMap $ M.toList wakeTimesToPurge
       where
        -- Note: some of the map values may become empty lists.
        -- But we shall not worry about cleaning those up here;
        -- they will be "garbage collected" as a matter of course
        -- when their tick comes up in "wakeUpRobotsDoneSleeping".
        f (k, botsToRemove) = M.adjust (filter (`S.notMember` botsToRemove)) k

      -- Step 4: Re-add the watching bots to be awakened at the next tick:
      wakeableBotIds = map fst wakeTimes
      newWakeTime = addTicks 1 currentTick
      newInsertions = M.singleton newWakeTime wakeableBotIds

  -- NOTE: There are two "sources of truth" for the waiting state of robots:
  -- 1. In the GameState via "internalWaitingRobots"
  -- 2. In each robot, via the CESK machine state

  -- 1. Update the game state
  internalWaitingRobots .= M.unionWith (<>) filteredWaiting newInsertions

  -- 2. Update the machine of each robot
  forM_ wakeableBotIds $ \rid ->
    robotMap . at rid . _Just . machine %= \case
      Waiting _ c -> Waiting newWakeTime c
      x -> x

deleteRobot :: (Has (State GameState) sig m) => RID -> m ()
deleteRobot rn = do
  internalActiveRobots %= IS.delete rn
  mrobot <- robotMap . at rn <<.= Nothing
  mrobot `forM_` \robot -> do
    -- Delete the robot from the index of robots by location.
    removeRobotFromLocationMap (robot ^. robotLocation) rn

-- | Makes sure empty sets don't hang around in the
-- 'robotsByLocation' map.  We don't want a key with an
-- empty set at every location any robot has ever
-- visited!
removeRobotFromLocationMap ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  RID ->
  m ()
removeRobotFromLocationMap (Cosmic oldSubworld oldPlanar) rid =
  robotsByLocation %= M.update (tidyDelete rid) oldSubworld
 where
  deleteOne x = surfaceEmpty IS.null . IS.delete x

  tidyDelete robID =
    surfaceEmpty M.null . M.update (deleteOne robID) oldPlanar

------------------------------------------------------------
-- Initialization
------------------------------------------------------------

type LaunchParams a = ParameterizableLaunchParams CodeToRun a

-- | In this stage in the UI pipeline, both fields
-- have already been validated, and "Nothing" means
-- that the field is simply absent.
type ValidatedLaunchParams = LaunchParams Identity

-- | Record to pass information needed to create an initial
--   'GameState' record when starting a scenario.
data GameStateConfig = GameStateConfig
  { initNameParts :: NameGenerator
  , initEntities :: EntityMap
  , initRecipes :: [Recipe Entity]
  , initWorldMap :: WorldMap
  }

-- | Create an initial, fresh game state record when starting a new scenario.
initGameState :: GameStateConfig -> GameState
initGameState gsc =
  GameState
    { _creativeMode = False
    , _temporal =
        TemporalState
          { _gameStep = WorldTick
          , _runStatus = Running
          , _ticks = TickNumber 0
          , _robotStepsPerTick = defaultRobotStepsPerTick
          }
    , _winCondition = NoWinCondition
    , _winSolution = Nothing
    , _robotMap = IM.empty
    , _robotsByLocation = M.empty
    , _robotsWatching = mempty
    , _discovery =
        Discovery
          { _availableRecipes = mempty
          , _availableCommands = mempty
          , _allDiscoveredEntities = empty
          , _knownEntities = []
          , -- This does not need to be initialized with anything,
            -- since the master list of achievements is stored in UIState
            _gameAchievements = mempty
          , _structureRecognition = StructureRecognizer (RecognizerAutomatons [] mempty) emptyFoundStructures []
          }
    , _activeRobots = IS.empty
    , _waitingRobots = M.empty
    , _seed = 0
    , _randGen = mkStdGen 0
    , _robotNaming =
        RobotNaming
          { _nameGenerator = initNameParts gsc
          , _gensym = 0
          }
    , _recipesInfo =
        Recipes
          { _recipesOut = outRecipeMap (initRecipes gsc)
          , _recipesIn = inRecipeMap (initRecipes gsc)
          , _recipesCat = catRecipeMap (initRecipes gsc)
          }
    , _currentScenarioPath = Nothing
    , _landscape =
        Landscape
          { _worldNavigation = Navigation mempty mempty
          , _multiWorld = mempty
          , _entityMap = initEntities gsc
          , _worldScrollable = True
          }
    , _viewCenterRule = VCRobot 0
    , _viewCenter = defaultCosmicLocation
    , _needsRedraw = False
    , _gameControls =
        GameControls
          { _replStatus = REPLDone Nothing
          , _replNextValueIndex = 0
          , _inputHandler = Nothing
          , _initiallyRunCode = Nothing
          }
    , _messageInfo =
        Messages
          { _messageQueue = Empty
          , _lastSeenMessageTime = TickNumber (-1)
          , _announcementQueue = mempty
          }
    , _focusedRobotID = 0
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
ensureStructureIntact (FoundStructure (StructureWithGrid _ grid) upperLeft) =
  allM outer $ zip [0 ..] grid
 where
  outer (y, row) = allM (inner y) $ zip [0 ..] row
  inner y (x, cell) =
    fmap (== cell) $
      entityAt $
        upperLeft `offsetBy` V2 x (negate y)

mkRecognizer ::
  (Has (State GameState) sig m) =>
  StaticStructureInfo ->
  m StructureRecognizer
mkRecognizer structInfo@(StaticStructureInfo structDefs _) = do
  foundIntact <- mapM (sequenceA . (id &&& ensureStructureIntact)) allPlaced
  let fs = populateStaticFoundStructures . map fst . filter snd $ foundIntact
      foundIntactLog =
        IntactStaticPlacement $
          map (\(x, isIntact) -> (isIntact, (Structure.name . originalDefinition . structureWithGrid) x, upperLeftCorner x)) foundIntact
  return $ StructureRecognizer (mkAutomatons structDefs) fs [foundIntactLog]
 where
  allPlaced = lookupStaticPlacements structInfo

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

  preliminaryGameState =
    (initGameState gsc)
      { _focusedRobotID = baseID
      }
      & creativeMode .~ scenario ^. scenarioCreative
      & winCondition .~ theWinCondition
      & winSolution .~ scenario ^. scenarioSolution
      & robotMap .~ IM.fromList (map (view robotID &&& id) robotList')
      & robotsByLocation .~ M.map (groupRobotsByPlanarLocation . NE.toList) (groupRobotsBySubworld robotList')
      & internalActiveRobots .~ setOf (traverse . robotID) robotList'
      & discovery . availableCommands .~ Notifications 0 initialCommands
      & discovery . knownEntities .~ scenario ^. scenarioKnown
      & robotNaming . gensym .~ initGensym
      & seed .~ theSeed
      & randGen .~ mkStdGen theSeed
      & recipesInfo %~ modifyRecipesInfo
      & landscape . entityMap .~ em
      & landscape . worldNavigation .~ scenario ^. scenarioNavigation
      & landscape . multiWorld .~ genMultiWorld worldTuples theSeed
      -- TODO (#1370): Should we allow subworlds to have their own scrollability?
      -- Leaning toward no , but for now just adopt the root world scrollability
      -- as being universal.
      & landscape . worldScrollable .~ NE.head (scenario ^. scenarioWorlds) ^. to scrollable
      & viewCenterRule .~ VCRobot baseID
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

  groupRobotsBySubworld =
    binTuples . map (view (robotLocation . subworld) &&& id)

  groupRobotsByPlanarLocation rs =
    M.fromListWith
      IS.union
      (map (view (robotLocation . planar) &&& (IS.singleton . view robotID)) rs)

  em = initEntities gsc <> scenario ^. scenarioEntities
  baseID = 0
  (things, devices) = partition (null . view entityCapabilities) (M.elems (entitiesByName em))

  getCodeToRun (CodeToRun _ s) = s

  robotsByBasePrecedence = genRobotTemplates scenario worldTuples

  initialCodeToRun = getCodeToRun <$> toRun

  robotList =
    zipWith instantiateRobot [baseID ..] robotsByBasePrecedence
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

  initGensym = length robotList - 1
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
