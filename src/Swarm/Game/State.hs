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
  gameStep,
  winCondition,
  winSolution,
  gameAchievements,
  announcementQueue,
  runStatus,
  paused,
  robotMap,
  robotsByLocation,
  robotsAtLocation,
  robotsInArea,
  baseRobot,
  activeRobots,
  waitingRobots,
  availableRecipes,
  availableCommands,
  messageNotifications,
  allDiscoveredEntities,
  gensym,
  seed,
  randGen,
  adjList,
  nameList,
  entityMap,
  recipesOut,
  recipesIn,
  recipesReq,
  scenarios,
  currentScenarioPath,
  knownEntities,
  world,
  worldScrollable,
  viewCenterRule,
  viewCenter,
  needsRedraw,
  replStatus,
  replNextValueIndex,
  replWorking,
  replActiveType,
  messageQueue,
  lastSeenMessageTime,
  focusedRobotID,
  ticks,
  robotStepsPerTick,

  -- ** Notifications
  Notifications (..),
  notificationsCount,
  notificationsContent,

  -- ** GameState initialization
  initGameState,
  scenarioToGameState,
  initGameStateForScenario,
  classicGame0,
  CodeToRun (..),
  Sha1 (..),
  SolutionSource (..),
  getParsedInitialCode,

  -- * Utilities
  applyViewCenterRule,
  recalcViewCenter,
  modifyViewCenter,
  viewingRegion,
  unfocus,
  focusedRobot,
  RobotRange (..),
  focusedRange,
  clearFocusedRobotLogUpdated,
  addRobot,
  addTRobot,
  emitMessage,
  sleepUntil,
  sleepForever,
  wakeUpRobotsDoneSleeping,
  deleteRobot,
  activateRobot,
  toggleRunStatus,
  messageIsRecent,
  messageIsFromNearby,
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Arrow (Arrow ((&&&)), left)
import Control.Effect.Lens
import Control.Effect.State (State)
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Data.Aeson (FromJSON, ToJSON)
import Data.Array (Array, listArray)
import Data.Bifunctor (first)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.IntSet.Lens (setOf)
import Data.List (partition, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T (drop, lines, pack, take, unlines)
import Data.Text.IO qualified as T (readFile)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (getZonedTime)
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.CESK (emptyStore, finalValue, initMachine)
import Swarm.Game.Entity
import Swarm.Game.Failure
import Swarm.Game.Failure.Render
import Swarm.Game.Location
import Swarm.Game.Recipe (
  Recipe,
  inRecipeMap,
  loadRecipes,
  outRecipeMap,
  reqRecipeMap,
 )
import Swarm.Game.ResourceLoading (getDataFileNameSafe)
import Swarm.Game.Robot
import Swarm.Game.Scenario.Objective
import Swarm.Game.ScenarioInfo
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.World (Coords (..), WorldFun (..), locToCoords, worldFunFromArray)
import Swarm.Game.World qualified as W
import Swarm.Game.WorldGen (Seed, findGoodOrigin, testWorld2FromArray)
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Module (Module (Module))
import Swarm.Language.Pipeline (ProcessedTerm (ProcessedTerm), processTermEither)
import Swarm.Language.Syntax (Const, SrcLoc (..), Syntax' (..), allConst)
import Swarm.Language.Typed (Typed (Typed))
import Swarm.Language.Types
import Swarm.Language.Value (Value)
import Swarm.Util (uniq, (<+=), (<<.=), (?))
import System.Clock qualified as Clock
import System.Random (StdGen, mkStdGen, randomRIO)

------------------------------------------------------------
-- Subsidiary data types
------------------------------------------------------------

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = -- | The view should be centered on an absolute position.
    VCLocation Location
  | -- | The view should be centered on a certain robot.
    VCRobot RID
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makePrisms ''ViewCenterRule

-- | A data type to represent the current status of the REPL.
data REPLStatus
  = -- | The REPL is not doing anything actively at the moment.
    --   We persist the last value and its type though.
    --   INVARIANT: the Value stored here is not a VResult.
    REPLDone (Maybe (Typed Value))
  | -- | A command entered at the REPL is currently being run.  The
    --   'Polytype' represents the type of the expression that was
    --   entered.  The @Maybe Value@ starts out as @Nothing@ and gets
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
  toSamples _ = SD.noSamples

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
--   also tracks time between ticks - use 'Swarm.TUI.Controller.safeTogglePause' instead.
toggleRunStatus :: RunStatus -> RunStatus
toggleRunStatus s = if s == Running then ManualPause else Running

-- | A data type to keep track of discovered recipes and commands
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
    PlayerAuthored Sha1

data CodeToRun = CodeToRun SolutionSource ProcessedTerm

getParsedInitialCode :: Maybe FilePath -> ExceptT Text IO (Maybe CodeToRun)
getParsedInitialCode toRun = case toRun of
  Nothing -> return Nothing
  Just filepath -> do
    contents <- liftIO $ TIO.readFile filepath
    pt@(ProcessedTerm (Module (Syntax' srcLoc _ _) _) _ _) <-
      ExceptT $
        return $
          left T.pack $
            processTermEither contents
    let strippedText = stripSrc srcLoc contents
        programBytestring = TL.encodeUtf8 $ TL.fromStrict strippedText
        sha1Hash = showDigest $ sha1 programBytestring
    return $ Just $ CodeToRun (PlayerAuthored $ Sha1 sha1Hash) pt
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

-- | Type for remebering which robots will be run next in a robot step mode.
--
-- Once some robots have run, we need to store RID to know which ones should go next.
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

-- | The main record holding the state for the game itself (as
--   distinct from the UI).  See the lenses below for access to its
--   fields.
data GameState = GameState
  { _creativeMode :: Bool
  , _gameStep :: Step
  , _winCondition :: WinCondition
  , _winSolution :: Maybe ProcessedTerm
  , _gameAchievements :: Map GameplayAchievement Attainment
  , _announcementQueue :: Seq Announcement
  , _runStatus :: RunStatus
  , _robotMap :: IntMap Robot
  , -- A set of robots to consider for the next game tick. It is guaranteed to
    -- be a subset of the keys of robotMap. It may contain waiting or idle
    -- robots. But robots that are present in robotMap and not in activeRobots
    -- are guaranteed to be either waiting or idle.
    _activeRobots :: IntSet
  , -- A set of probably waiting robots, indexed by probable wake-up time. It
    -- may contain robots that are in fact active or idle, as well as robots
    -- that do not exist anymore. Its only guarantee is that once a robot name
    -- with its wake up time is inserted in it, it will remain there until the
    -- wake-up time is reached, at which point it is removed via
    -- wakeUpRobotsDoneSleeping.
    -- Waiting robots for a given time are a list because it is cheaper to
    -- append to a list than to a Set.
    _waitingRobots :: Map Integer [RID]
  , _robotsByLocation :: Map Location IntSet
  , _allDiscoveredEntities :: Inventory
  , _availableRecipes :: Notifications (Recipe Entity)
  , _availableCommands :: Notifications Const
  , _gensym :: Int
  , _seed :: Seed
  , _randGen :: StdGen
  , _adjList :: Array Int Text
  , _nameList :: Array Int Text
  , _entityMap :: EntityMap
  , _recipesOut :: IntMap [Recipe Entity]
  , _recipesIn :: IntMap [Recipe Entity]
  , _recipesReq :: IntMap [Recipe Entity]
  , _scenarios :: ScenarioCollection
  , _currentScenarioPath :: Maybe FilePath
  , _knownEntities :: [Text]
  , _world :: W.World Int Entity
  , _worldScrollable :: Bool
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter :: Location
  , _needsRedraw :: Bool
  , _replStatus :: REPLStatus
  , _replNextValueIndex :: Integer
  , _messageQueue :: Seq LogEntry
  , _lastSeenMessageTime :: Integer
  , _focusedRobotID :: RID
  , _ticks :: Integer
  , _robotStepsPerTick :: Int
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

let exclude = ['_viewCenter, '_focusedRobotID, '_viewCenterRule, '_activeRobots, '_waitingRobots, '_adjList, '_nameList]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''GameState

-- | Is the user in creative mode (i.e. able to do anything without restriction)?
creativeMode :: Lens' GameState Bool

-- | How to step the game - 'WorldTick' or 'RobotStep' for debugging the 'CESK' machine.
gameStep :: Lens' GameState Step

-- | How to determine whether the player has won.
winCondition :: Lens' GameState WinCondition

-- | How to win (if possible). This is useful for automated testing
--   and to show help to cheaters (or testers).
winSolution :: Lens' GameState (Maybe ProcessedTerm)

-- | Map of in-game achievements that were attained
gameAchievements :: Lens' GameState (Map GameplayAchievement Attainment)

-- | A queue of global announcments.
-- Note that this is distinct from the "messageQueue",
-- which is for messages emitted by robots.
--
-- Note that we put the newest entry to the right.
announcementQueue :: Lens' GameState (Seq Announcement)

-- | The current 'RunStatus'.
runStatus :: Lens' GameState RunStatus

-- | Whether the game is currently paused.
paused :: Getter GameState Bool
paused = to (\s -> s ^. runStatus /= Running)

-- | All the robots that currently exist in the game, indexed by ID.
robotMap :: Lens' GameState (IntMap Robot)

-- | The names of all robots that currently exist in the game, indexed by
--   location (which we need both for /e.g./ the 'Salvage' command as
--   well as for actually drawing the world).  Unfortunately there is
--   no good way to automatically keep this up to date, since we don't
--   just want to completely rebuild it every time the 'robotMap'
--   changes.  Instead, we just make sure to update it every time the
--   location of a robot changes, or a robot is created or destroyed.
--   Fortunately, there are relatively few ways for these things to
--   happen.
robotsByLocation :: Lens' GameState (Map Location IntSet)

-- | Get a list of all the robots at a particular location.
robotsAtLocation :: Location -> GameState -> [Robot]
robotsAtLocation loc gs =
  mapMaybe (`IM.lookup` (gs ^. robotMap))
    . maybe [] IS.toList
    . M.lookup loc
    . view robotsByLocation
    $ gs

-- | Get all the robots within a given Manhattan distance from a
--   location.
robotsInArea :: Location -> Int32 -> GameState -> [Robot]
robotsInArea o d gs = map (rm IM.!) rids
 where
  rm = gs ^. robotMap
  rl = gs ^. robotsByLocation
  rids = concatMap IS.elems $ getElemsInArea o d rl

-- | The base robot, if it exists.
baseRobot :: Traversal' GameState Robot
baseRobot = robotMap . ix 0

-- | The list of entities that have been discovered.
allDiscoveredEntities :: Lens' GameState Inventory

-- | The list of available recipes.
availableRecipes :: Lens' GameState (Notifications (Recipe Entity))

-- | The list of available commands.
availableCommands :: Lens' GameState (Notifications Const)

-- | The names of the robots that are currently not sleeping.
activeRobots :: Getter GameState IntSet
activeRobots = internalActiveRobots

-- | The names of the robots that are currently sleeping, indexed by wake up
--   time. Note that this may not include all sleeping robots, particularly
--   those that are only taking a short nap (e.g. wait 1).
waitingRobots :: Getter GameState (Map Integer [RID])
waitingRobots = internalWaitingRobots

-- | A counter used to generate globally unique IDs.
gensym :: Lens' GameState Int

-- | The initial seed that was used for the random number generator,
--   and world generation.
seed :: Lens' GameState Seed

-- | Pseudorandom generator initialized at start.
randGen :: Lens' GameState StdGen

-- | Read-only list of words, for use in building random robot names.
adjList :: Getter GameState (Array Int Text)
adjList = to _adjList

-- | Read-only list of words, for use in building random robot names.
nameList :: Getter GameState (Array Int Text)
nameList = to _nameList

-- | The catalog of all entities that the game knows about.
entityMap :: Lens' GameState EntityMap

-- | All recipes the game knows about, indexed by outputs.
recipesOut :: Lens' GameState (IntMap [Recipe Entity])

-- | All recipes the game knows about, indexed by inputs.
recipesIn :: Lens' GameState (IntMap [Recipe Entity])

-- | All recipes the game knows about, indexed by requirement/catalyst.
recipesReq :: Lens' GameState (IntMap [Recipe Entity])

-- | The collection of scenarios that comes with the game.
scenarios :: Lens' GameState ScenarioCollection

-- | The filepath of the currently running scenario.
--
-- This is useful as an index to 'scenarios' collection,
-- see 'Swarm.Game.ScenarioInfo.scenarioItemByPath'.
currentScenarioPath :: Lens' GameState (Maybe FilePath)

-- | The names of entities that should be considered "known", that is,
--   robots know what they are without having to scan them.
knownEntities :: Lens' GameState [Text]

-- | The current state of the world (terrain and entities only; robots
--   are stored in the 'robotMap').  Int is used instead of
--   TerrainType because we need to be able to store terrain values in
--   unboxed tile arrays.
world :: Lens' GameState (W.World Int Entity)

-- | Whether the world map is supposed to be scrollable or not.
worldScrollable :: Lens' GameState Bool

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter GameState Location
viewCenter = to _viewCenter

-- | Whether the world view needs to be redrawn.
needsRedraw :: Lens' GameState Bool

-- | The current status of the REPL.
replStatus :: Lens' GameState REPLStatus

-- | The index of the next it{index} value
replNextValueIndex :: Lens' GameState Integer

-- | A queue of global messages.
--
-- Note that we put the newest entry to the right.
messageQueue :: Lens' GameState (Seq LogEntry)

-- | Last time message queue has been viewed (used for notification).
lastSeenMessageTime :: Lens' GameState Integer

-- | The current robot in focus.
--
-- It is only a 'Getter' because this value should be updated only when
-- the 'viewCenterRule' is specified to be a robot.
--
-- Technically it's the last robot ID specified by 'viewCenterRule',
-- but that robot may not be alive anymore - to be safe use 'focusedRobot'.
focusedRobotID :: Getter GameState RID
focusedRobotID = to _focusedRobotID

-- | The number of ticks elapsed since the game started.
ticks :: Lens' GameState Integer

-- | The maximum number of CESK machine steps a robot may take during
--   a single tick.
robotStepsPerTick :: Lens' GameState Int

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | The current rule for determining the center of the world view.
--   It updates also, viewCenter and focusedRobotName to keep
--   everything synchronize.
viewCenterRule :: Lens' GameState ViewCenterRule
viewCenterRule = lens getter setter
 where
  getter :: GameState -> ViewCenterRule
  getter = _viewCenterRule

  -- The setter takes care of updating viewCenter and focusedRobotName
  -- So non of this fields get out of sync.
  setter :: GameState -> ViewCenterRule -> GameState
  setter g rule =
    case rule of
      VCLocation v2 -> g {_viewCenterRule = rule, _viewCenter = v2}
      VCRobot rid ->
        let robotcenter = g ^? robotMap . ix rid . robotLocation
         in -- retrieve the loc of the robot if it exists, Nothing otherwise.
            -- sometimes, lenses are amazing...
            case robotcenter of
              Nothing -> g
              Just v2 -> g {_viewCenterRule = rule, _viewCenter = v2, _focusedRobotID = rid}

-- | Whether the repl is currently working.
replWorking :: Getter GameState Bool
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
    new = takeWhile (\l -> l ^. leTime > gs ^. lastSeenMessageTime) $ reverse allUniq
    -- creative players and system robots just see all messages (and focused robots logs)
    unchecked = gs ^. creativeMode || fromMaybe False (focusedRobot gs ^? _Just . systemRobot)
    messages = (if unchecked then id else focusedOrLatestClose) (gs ^. messageQueue)
    allMessages = Seq.sort $ focusedLogs <> messages
    focusedLogs = maybe Empty (view robotLog) (focusedRobot gs)
    -- classic players only get to see messages that they said and a one message that they just heard
    -- other they have to get from log
    latestMsg = messageIsRecent gs
    closeMsg = messageIsFromNearby (gs ^. viewCenter)
    focusedOrLatestClose mq =
      (Seq.take 1 . Seq.reverse . Seq.filter closeMsg $ Seq.takeWhileR latestMsg mq)
        <> Seq.filter ((== gs ^. focusedRobotID) . view leRobotID) mq

messageIsRecent :: GameState -> LogEntry -> Bool
messageIsRecent gs e = e ^. leTime >= gs ^. ticks - 1

messageIsFromNearby :: Location -> LogEntry -> Bool
messageIsFromNearby l e = manhattan l (e ^. leLocation) <= hearingDistance

-- | Given a current mapping from robot names to robots, apply a
--   'ViewCenterRule' to derive the location it refers to.  The result
--   is @Maybe@ because the rule may refer to a robot which does not
--   exist.
applyViewCenterRule :: ViewCenterRule -> IntMap Robot -> Maybe Location
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation

-- | Recalculate the veiw center (and cache the result in the
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
  newViewCenter = fromMaybe oldViewCenter (applyViewCenterRule (g ^. viewCenterRule) (g ^. robotMap))

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter :: (Location -> Location) -> GameState -> GameState
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
viewingRegion :: GameState -> (Int32, Int32) -> (W.Coords, W.Coords)
viewingRegion g (w, h) = (W.Coords (rmin, cmin), W.Coords (rmax, cmax))
 where
  Location cx cy = g ^. viewCenter
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
--       - If the robot is within the minimum radius, it is 'Close'.
--       - If the robot is between the minimum and maximum radii, it
--         is 'MidRange', with a 'Double' value ranging linearly from
--         0 to 1 proportional to the distance from the minimum to
--         maximum radius.  For example, 'MidRange 0.5' would indicate
--         a robot exactly halfway between the minimum and maximum
--         radii.
--       - If the robot is beyond the maximum radius, it is 'Far'.
--   * By default, the minimum radius is 16, and maximum is 64.
--   * If the focused robot has an @antenna@ installed, it doubles
--     both radii.
--   * If the base has an @antenna@ installed, it also doubles both radii.
focusedRange :: GameState -> Maybe RobotRange
focusedRange g = computedRange <$ focusedRobot g
 where
  computedRange
    | g ^. creativeMode || g ^. worldScrollable || r <= minRadius = Close
    | r > maxRadius = Far
    | otherwise = MidRange $ (r - minRadius) / (maxRadius - minRadius)

  -- Euclidean distance from the base to the view center.
  r = case g ^. robotMap . at 0 of
    Just br -> euclidean (g ^. viewCenter) (br ^. robotLocation)
    _ -> 1000000000 -- if the base doesn't exist, we have bigger problems

  -- See whether the base or focused robot have antennas installed.
  baseInv, focInv :: Maybe Inventory
  baseInv = g ^? robotMap . ix 0 . equippedDevices
  focInv = view equippedDevices <$> focusedRobot g

  gain :: Maybe Inventory -> (Double -> Double)
  gain (Just inv)
    | countByName "antenna" inv > 0 = (* 2)
  gain _ = id

  -- Range radii.  Default thresholds are 16, 64; each antenna
  -- boosts the signal by 2x.
  minRadius, maxRadius :: Double
  (minRadius, maxRadius) = over both (gain baseInv . gain focInv) (16, 64)

-- | Clear the 'robotLogUpdated' flag of the focused robot.
clearFocusedRobotLogUpdated :: Has (State GameState) sig m => m ()
clearFocusedRobotLogUpdated = do
  n <- use focusedRobotID
  robotMap . ix n . robotLogUpdated .= False

-- | Add a concrete instance of a robot template to the game state:
--   first, generate a unique ID number for it.  Then, add it to the
--   main robot map, the active robot set, and to to the index of
--   robots by location. Return the updated robot.
addTRobot :: Has (State GameState) sig m => TRobot -> m Robot
addTRobot r = do
  rid <- gensym <+= 1
  let r' = instantiateRobot rid r
  addRobot r'
  return r'

-- | Add a robot to the game state, adding it to the main robot map,
--   the active robot set, and to to the index of robots by
--   location.
addRobot :: Has (State GameState) sig m => Robot -> m ()
addRobot r = do
  let rid = r ^. robotID

  robotMap %= IM.insert rid r
  robotsByLocation
    %= M.insertWith IS.union (r ^. robotLocation) (IS.singleton rid)
  internalActiveRobots %= IS.insert rid

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

-- | Add a message to the message queue.
emitMessage :: Has (State GameState) sig m => LogEntry -> m ()
emitMessage msg = messageQueue %= (|> msg) . dropLastIfLong
 where
  tooLong s = Seq.length s >= maxMessageQueueSize
  dropLastIfLong whole@(_oldest :<| newer) = if tooLong whole then newer else whole
  dropLastIfLong emptyQueue = emptyQueue

-- | Takes a robot out of the activeRobots set and puts it in the waitingRobots
--   queue.
sleepUntil :: Has (State GameState) sig m => RID -> Integer -> m ()
sleepUntil rid time = do
  internalActiveRobots %= IS.delete rid
  internalWaitingRobots . at time . non [] %= (rid :)

-- | Takes a robot out of the activeRobots set.
sleepForever :: Has (State GameState) sig m => RID -> m ()
sleepForever rid = internalActiveRobots %= IS.delete rid

-- | Adds a robot to the activeRobots set.
activateRobot :: Has (State GameState) sig m => RID -> m ()
activateRobot rid = internalActiveRobots %= IS.insert rid

-- | Removes robots whose wake up time matches the current game ticks count
--   from the waitingRobots queue and put them back in the activeRobots set
--   if they still exist in the keys of robotMap.
wakeUpRobotsDoneSleeping :: Has (State GameState) sig m => m ()
wakeUpRobotsDoneSleeping = do
  time <- use ticks
  mrids <- internalWaitingRobots . at time <<.= Nothing
  case mrids of
    Nothing -> return ()
    Just rids -> do
      robots <- use robotMap
      let aliveRids = filter (`IM.member` robots) rids
      internalActiveRobots %= IS.union (IS.fromList aliveRids)

deleteRobot :: Has (State GameState) sig m => RID -> m ()
deleteRobot rn = do
  internalActiveRobots %= IS.delete rn
  mrobot <- robotMap . at rn <<.= Nothing
  mrobot `forM_` \robot -> do
    -- Delete the robot from the index of robots by location.
    robotsByLocation . ix (robot ^. robotLocation) %= IS.delete rn

------------------------------------------------------------
-- Initialization
------------------------------------------------------------

-- | Create an initial game state record, first loading entities and
--   recipes from disk.
initGameState :: ExceptT Text IO ([SystemFailure], GameState)
initGameState = do
  entities <- ExceptT loadEntities
  recipes <- withExceptT prettyFailure $ loadRecipes entities
  eitherLoadedScenarios <- liftIO $ runExceptT $ loadScenarios entities
  let (scenarioWarnings, loadedScenarios) = case eitherLoadedScenarios of
        Left xs -> (xs, SC mempty mempty)
        Right (warnings, x) -> (warnings, x)

  (adjsFile, namesFile) <- withExceptT prettyFailure $ do
    adjsFile <- getDataFileNameSafe NameGeneration "adjectives.txt"
    namesFile <- getDataFileNameSafe NameGeneration "names.txt"
    return (adjsFile, namesFile)

  let markEx what a = catchError a (\e -> fail $ "Failed to " <> what <> ": " <> show e)
  (adjs, names) <- liftIO . markEx "load name generation data" $ do
    as <- tail . T.lines <$> T.readFile adjsFile
    ns <- tail . T.lines <$> T.readFile namesFile
    return (as, ns)

  return
    ( scenarioWarnings
    , GameState
        { _creativeMode = False
        , _gameStep = WorldTick
        , _winCondition = NoWinCondition
        , _winSolution = Nothing
        , -- This does not need to be initialized with anything,
          -- since the master list of achievements is stored in UIState
          _gameAchievements = mempty
        , _announcementQueue = mempty
        , _runStatus = Running
        , _robotMap = IM.empty
        , _robotsByLocation = M.empty
        , _availableRecipes = mempty
        , _availableCommands = mempty
        , _allDiscoveredEntities = empty
        , _activeRobots = IS.empty
        , _waitingRobots = M.empty
        , _gensym = 0
        , _seed = 0
        , _randGen = mkStdGen 0
        , _adjList = listArray (0, length adjs - 1) adjs
        , _nameList = listArray (0, length names - 1) names
        , _entityMap = entities
        , _recipesOut = outRecipeMap recipes
        , _recipesIn = inRecipeMap recipes
        , _recipesReq = reqRecipeMap recipes
        , _scenarios = loadedScenarios
        , _currentScenarioPath = Nothing
        , _knownEntities = []
        , _world = W.emptyWorld (fromEnum StoneT)
        , _worldScrollable = True
        , _viewCenterRule = VCRobot 0
        , _viewCenter = origin
        , _needsRedraw = False
        , _replStatus = REPLDone Nothing
        , _replNextValueIndex = 0
        , _messageQueue = Empty
        , _lastSeenMessageTime = -1
        , _focusedRobotID = 0
        , _ticks = 0
        , _robotStepsPerTick = defaultRobotStepsPerTick
        }
    )

-- | Set a given scenario as the currently loaded scenario in the game state.
scenarioToGameState :: Scenario -> Maybe Seed -> Maybe CodeToRun -> GameState -> IO GameState
scenarioToGameState scenario userSeed toRun g = do
  -- Decide on a seed.  In order of preference, we will use:
  --   1. seed value provided by the user
  --   2. seed value specified in the scenario description
  --   3. randomly chosen seed value
  theSeed <- case userSeed <|> scenario ^. scenarioSeed of
    Just s -> return s
    Nothing -> randomRIO (0, maxBound :: Int)

  now <- Clock.getTime Clock.Monotonic
  let robotList' = (robotCreatedAt .~ now) <$> robotList

  return $
    g
      { _creativeMode = scenario ^. scenarioCreative
      , _winCondition = theWinCondition
      , _winSolution = scenario ^. scenarioSolution
      , _runStatus = Running
      , _robotMap = IM.fromList $ map (view robotID &&& id) robotList'
      , _robotsByLocation =
          M.fromListWith IS.union $
            map (view robotLocation &&& (IS.singleton . view robotID)) robotList'
      , _activeRobots = setOf (traverse . robotID) robotList'
      , _availableCommands = Notifications 0 initialCommands
      , _waitingRobots = M.empty
      , _gensym = initGensym
      , _seed = theSeed
      , _randGen = mkStdGen theSeed
      , _entityMap = em
      , _recipesOut = addRecipesWith outRecipeMap recipesOut
      , _recipesIn = addRecipesWith inRecipeMap recipesIn
      , _recipesReq = addRecipesWith reqRecipeMap recipesReq
      , _knownEntities = scenario ^. scenarioKnown
      , _world = theWorld theSeed
      , _worldScrollable = scenario ^. scenarioWorld . to scrollable
      , _viewCenterRule = VCRobot baseID
      , _viewCenter = origin
      , _needsRedraw = False
      , -- When the base starts out running a program, the REPL status must be set to working,
        -- otherwise the store of definition cells is not saved (see #333, #838)
        _replStatus = case running of
          False -> REPLDone Nothing
          True -> REPLWorking (Typed Nothing PolyUnit mempty)
      , _replNextValueIndex = 0
      , _messageQueue = Empty
      , _focusedRobotID = baseID
      , _ticks = 0
      , _robotStepsPerTick = (scenario ^. scenarioStepsPerTick) ? defaultRobotStepsPerTick
      }
 where
  em = g ^. entityMap <> scenario ^. scenarioEntities

  baseID = 0
  (things, devices) = partition (null . view entityCapabilities) (M.elems (entitiesByName em))
  -- Keep only robots from the robot list with a concrete location;
  -- the others existed only to serve as a template for robots drawn
  -- in the world map
  locatedRobots = filter (isJust . view trobotLocation) $ scenario ^. scenarioRobots
  getCodeToRun (CodeToRun _ s) = s

  -- Rules for selecting the "base" robot:
  -- -------------------------------------
  -- What follows is a thorough description of how the base
  -- choice is made as of the most recent study of the code.
  -- This level of detail is not meant to be public-facing.
  --
  -- For an abbreviated explanation, see the "Base robot" section of the
  -- "Scenario Authoring Guide".
  -- https://github.com/swarm-game/swarm/tree/main/data/scenarios#base-robot
  --
  -- Precedence rules:
  -- 1. Prefer those robots defined with a loc in the Scenario file
  --   1.a. If multiple robots define a loc, use the robot that is defined
  --        first within the Scenario file.
  --   1.b. Note that if a robot is both given a loc AND is specified in the
  --        world map, then two instances of the robot shall be created. The
  --        instance with the loc shall be preferred as the base.
  -- 2. Fall back to robots generated from templates via the map and palette.
  --   2.a. If multiple robots are specified in the map, prefer the one that
  --        is defined first within the Scenario file.
  --   2.b. If multiple robots are instantiated from the same template, then
  --        prefer the one closest to the upper-left of the screen, with higher rows given precedence over columns.
  robotsByBasePrecedence = locatedRobots ++ map snd (sortOn fst genRobots)

  robotList =
    zipWith instantiateRobot [baseID ..] robotsByBasePrecedence
      -- If the  --run flag was used, use it to replace the CESK machine of the
      -- robot whose id is 0, i.e. the first robot listed in the scenario.
      -- Note that this *replaces* any program the base robot otherwise
      -- would have run (i.e. any program specified in the program: field
      -- of the scenario description).
      & ix baseID . machine
        %~ case getCodeToRun <$> toRun of
          Nothing -> id
          Just pt -> const $ initMachine pt Ctx.empty emptyStore
      -- If we are in creative mode, give base all the things
      & ix baseID . robotInventory
        %~ case scenario ^. scenarioCreative of
          False -> id
          True -> union (fromElems (map (0,) things))
      & ix baseID . equippedDevices
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

  (genRobots, wf) = buildWorld em (scenario ^. scenarioWorld)
  theWorld = W.newWorld . wf
  theWinCondition =
    maybe
      NoWinCondition
      (\x -> WinConditions Ongoing (ObjectiveCompletion (CompletionBuckets (NE.toList x) mempty mempty) mempty))
      (NE.nonEmpty (scenario ^. scenarioObjectives))

  initGensym = length robotList - 1
  addRecipesWith f gRs = IM.unionWith (<>) (f $ scenario ^. scenarioRecipes) (g ^. gRs)

-- | Take a world description, parsed from a scenario file, and turn
--   it into a list of located robots and a world function.
buildWorld :: EntityMap -> WorldDescription -> ([IndexedTRobot], Seed -> WorldFun Int Entity)
buildWorld em WorldDescription {..} = (robots, first fromEnum . wf)
 where
  rs = fromIntegral $ length area
  cs = fromIntegral $ length (head area)
  Coords (ulr, ulc) = locToCoords ul

  worldGrid :: [[(TerrainType, Maybe Entity)]]
  worldGrid = (map . map) (cellTerrain &&& cellEntity) area

  worldArray :: Array (Int32, Int32) (TerrainType, Maybe Entity)
  worldArray = listArray ((ulr, ulc), (ulr + rs - 1, ulc + cs - 1)) (concat worldGrid)

  wf = case defaultTerrain of
    Nothing ->
      (if offsetOrigin then findGoodOrigin else id) . testWorld2FromArray em worldArray
    Just (Cell t e _) -> const (worldFunFromArray worldArray (t, e))

  -- Get all the robots described in cells and set their locations appropriately
  robots :: [IndexedTRobot]
  robots =
    area
      & traversed Control.Lens.<.> traversed %@~ (,) -- add (r,c) indices
      & concat
      & concatMap
        ( \((fromIntegral -> r, fromIntegral -> c), Cell _ _ robotList) ->
            let robotWithLoc = trobotLocation ?~ W.coordsToLoc (Coords (ulr + r, ulc + c))
             in map (fmap robotWithLoc) robotList
        )

-- | Create an initial game state for a specific scenario.
-- Note that this function is used only for unit tests, integration tests, and benchmarks.
--
-- In normal play, the code path that gets executed is scenarioToAppState.
initGameStateForScenario ::
  String ->
  Maybe Seed ->
  Maybe FilePath ->
  ExceptT Text IO GameState
initGameStateForScenario sceneName userSeed toRun = do
  (warnings, g) <- initGameState
  unless (null warnings)
    . except
    . Left
    . T.unlines
    . map prettyFailure
    $ warnings
  (scene, path) <- loadScenario sceneName (g ^. entityMap)
  maybeRunScript <- getParsedInitialCode toRun
  gs <- liftIO $ scenarioToGameState scene userSeed maybeRunScript g
  normalPath <- liftIO $ normalizeScenarioPath (gs ^. scenarios) path
  t <- liftIO getZonedTime
  return $
    gs
      & currentScenarioPath ?~ normalPath
      & scenarios . scenarioItemByPath normalPath . _SISingle . _2 . scenarioStatus .~ InProgress t 0 0

-- | For convenience, the 'GameState' corresponding to the classic
--   game with seed 0.
--   This is used only for benchmarks and unit tests.
classicGame0 :: ExceptT Text IO GameState
classicGame0 = initGameStateForScenario "classic" (Just 0) Nothing
