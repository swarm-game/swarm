{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Swarm.Game.State
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definition of the record holding all the game-related state, and various related
-- utility functions.
module Swarm.Game.State (
  -- * Game state record and related types
  ViewCenterRule (..),
  REPLStatus (..),
  WinCondition (..),
  _NoWinCondition,
  _WinConditions,
  _Won,
  RunStatus (..),
  Seed,
  GameState,

  -- ** GameState fields
  creativeMode,
  winCondition,
  winSolution,
  runStatus,
  paused,
  robotMap,
  robotsByLocation,
  robotsAtLocation,
  robotsInArea,
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
  viewCenterRule,
  viewCenter,
  needsRedraw,
  replStatus,
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

  -- * Utilities
  applyViewCenterRule,
  recalcViewCenter,
  modifyViewCenter,
  viewingRegion,
  focusedRobot,
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
import Control.Arrow (Arrow ((&&&)))
import Control.Effect.Lens
import Control.Effect.State (State)
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad.Except
import Data.Aeson (FromJSON, ToJSON)
import Data.Array (Array, listArray)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.IntSet.Lens (setOf)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T (lines)
import Data.Text.IO qualified as T (readFile)
import Data.Time (getZonedTime)
import GHC.Generics (Generic)
import Linear
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Entity
import Swarm.Game.Recipe (
  Recipe,
  inRecipeMap,
  loadRecipes,
  outRecipeMap,
  reqRecipeMap,
 )
import Swarm.Game.Robot
import Swarm.Game.ScenarioInfo
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.Value (Value)
import Swarm.Game.World (Coords (..), WorldFun (..), locToCoords, worldFunFromArray)
import Swarm.Game.World qualified as W
import Swarm.Game.WorldGen (Seed, findGoodOrigin, testWorld2FromArray)
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (Const, Term (TText), allConst)
import Swarm.Language.Typed (Typed (Typed))
import Swarm.Language.Types
import Swarm.Util (getDataFileNameSafe, getElemsInArea, isRightOr, manhattan, uniq, (<+=), (<<.=), (?))
import System.Clock qualified as Clock
import System.Random (StdGen, mkStdGen, randomRIO)
import Witch (into)

------------------------------------------------------------
-- Subsidiary data types
------------------------------------------------------------

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = -- | The view should be centered on an absolute position.
    VCLocation (V2 Int64)
  | -- | The view should be centered on a certain robot.
    VCRobot RID
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

makePrisms ''ViewCenterRule

-- | A data type to represent the current status of the REPL.
data REPLStatus
  = -- | The REPL is not doing anything actively at the moment.
    --   We persist the last value and its type though.
    REPLDone (Maybe (Typed Value))
  | -- | A command entered at the REPL is currently being run.  The
    --   'Polytype' represents the type of the expression that was
    --   entered.  The @Maybe Value@ starts out as @Nothing@ and gets
    --   filled in with a result once the command completes.
    REPLWorking (Typed (Maybe Value))
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data WinCondition
  = -- | There is no winning condition.
    NoWinCondition
  | -- | There are one or more objectives remaining that the player
    --   has not yet accomplished.
    WinConditions (NonEmpty Objective)
  | -- | The player has won. The boolean indicates whether they have
    --   already been congratulated.
    Won Bool
  deriving (Show, Generic, FromJSON, ToJSON)

makePrisms ''WinCondition

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

------------------------------------------------------------
-- The main GameState record type
------------------------------------------------------------

-- | By default, robots may make a maximum of 100 CESK machine steps
--   during one game tick.
defaultRobotStepsPerTick :: Int
defaultRobotStepsPerTick = 100

-- | The main record holding the state for the game itself (as
--   distinct from the UI).  See the lenses below for access to its
--   fields.
data GameState = GameState
  { _creativeMode :: Bool
  , _winCondition :: WinCondition
  , _winSolution :: Maybe ProcessedTerm
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
  , _robotsByLocation :: Map (V2 Int64) IntSet
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
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter :: V2 Int64
  , _needsRedraw :: Bool
  , _replStatus :: REPLStatus
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

-- | How to determine whether the player has won.
winCondition :: Lens' GameState WinCondition

-- | How to win (if possible). This is useful for automated testing
--   and to show help to cheaters (or testers).
winSolution :: Lens' GameState (Maybe ProcessedTerm)

-- | The current 'RunStatus'.
runStatus :: Lens' GameState RunStatus

-- | Whether the game is currently paused.
paused :: Getter GameState Bool
paused = to (\s -> s ^. runStatus /= Running)

-- | All the robots that currently exist in the game, indexed by name.
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
robotsByLocation :: Lens' GameState (Map (V2 Int64) IntSet)

-- | Get a list of all the robots at a particular location.
robotsAtLocation :: V2 Int64 -> GameState -> [Robot]
robotsAtLocation loc gs =
  mapMaybe (`IM.lookup` (gs ^. robotMap))
    . maybe [] IS.toList
    . M.lookup loc
    . view robotsByLocation
    $ gs

-- | Get robots in manhattan distastance from location.
robotsInArea :: V2 Int64 -> Int64 -> GameState -> [Robot]
robotsInArea o d gs = map (rm IM.!) rids
 where
  rm = gs ^. robotMap
  rl = gs ^. robotsByLocation
  rids = concatMap IS.elems $ getElemsInArea o d rl

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

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter GameState (V2 Int64)
viewCenter = to _viewCenter

-- | Whether the world view needs to be redrawn.
needsRedraw :: Lens' GameState Bool

-- | The current status of the REPL.
replStatus :: Lens' GameState REPLStatus

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

messageIsFromNearby :: V2 Int64 -> LogEntry -> Bool
messageIsFromNearby l e = manhattan l (e ^. leLocation) <= hearingDistance

-- | Given a current mapping from robot names to robots, apply a
--   'ViewCenterRule' to derive the location it refers to.  The result
--   is @Maybe@ because the rule may refer to a robot which does not
--   exist.
applyViewCenterRule :: ViewCenterRule -> IntMap Robot -> Maybe (V2 Int64)
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
modifyViewCenter :: (V2 Int64 -> V2 Int64) -> GameState -> GameState
modifyViewCenter update g =
  g
    & case g ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _ -> viewCenterRule .~ VCLocation (update (g ^. viewCenter))

-- | Given a width and height, compute the region, centered on the
--   'viewCenter', that should currently be in view.
viewingRegion :: GameState -> (Int64, Int64) -> (W.Coords, W.Coords)
viewingRegion g (w, h) = (W.Coords (rmin, cmin), W.Coords (rmax, cmax))
 where
  V2 cx cy = g ^. viewCenter
  (rmin, rmax) = over both (+ (-cy - h `div` 2)) (0, h - 1)
  (cmin, cmax) = over both (+ (cx - w `div` 2)) (0, w - 1)

-- | Find out which robot has been last specified by the
--   'viewCenterRule', if any.
focusedRobot :: GameState -> Maybe Robot
focusedRobot g = g ^. robotMap . at (g ^. focusedRobotID)

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
--   recipies from disk.
initGameState :: ExceptT Text IO GameState
initGameState = do
  let guardRight what i = i `isRightOr` (\e -> "Failed to " <> what <> ": " <> e)
  entities <- loadEntities >>= guardRight "load entities"
  recipes <- loadRecipes entities >>= guardRight "load recipes"
  loadedScenarios <- loadScenarios entities >>= guardRight "load scenarios"

  let markEx what a = catchError a (\e -> fail $ "Failed to " <> what <> ": " <> show e)

  (adjs, names) <- liftIO . markEx "load name generation data" $ do
    -- if data directory did not exist we would have failed loading scenarios
    Just adjsFile <- getDataFileNameSafe "adjectives.txt"
    as <- tail . T.lines <$> T.readFile adjsFile
    Just namesFile <- getDataFileNameSafe "names.txt"
    ns <- tail . T.lines <$> T.readFile namesFile
    return (as, ns)

  return $
    GameState
      { _creativeMode = False
      , _winCondition = NoWinCondition
      , _winSolution = Nothing
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
      , _viewCenterRule = VCRobot 0
      , _viewCenter = V2 0 0
      , _needsRedraw = False
      , _replStatus = REPLDone Nothing
      , _messageQueue = Empty
      , _lastSeenMessageTime = -1
      , _focusedRobotID = 0
      , _ticks = 0
      , _robotStepsPerTick = defaultRobotStepsPerTick
      }

-- | Set a given scenario as the currently loaded scenario in the game state.
scenarioToGameState :: Scenario -> Maybe Seed -> Maybe String -> GameState -> IO GameState
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
      , _viewCenterRule = VCRobot baseID
      , _viewCenter = V2 0 0
      , _needsRedraw = False
      , -- When starting base with the run flag, REPL status must be set to working,
        -- otherwise the store of definition cells is not saved (see #333)
        _replStatus = case toRun of
          Nothing -> REPLDone Nothing
          Just _ -> REPLWorking (Typed Nothing PolyUnit mempty)
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
  robotList =
    zipWith instantiateRobot [baseID ..] (locatedRobots ++ genRobots)
      -- If the  --run flag was used, use it to replace the CESK machine of the
      -- robot whose id is 0, i.e. the first robot listed in the scenario.
      & ix baseID . machine
        %~ case toRun of
          Nothing -> id
          Just (into @Text -> f) -> const (initMachine [tmQ| run($str:f) |] Ctx.empty emptyStore)
      -- If we are in creative mode, give base all the things
      & ix baseID . robotInventory
        %~ case scenario ^. scenarioCreative of
          False -> id
          True -> union (fromElems (map (0,) things))
      & ix baseID . installedDevices
        %~ case scenario ^. scenarioCreative of
          False -> id
          True -> const (fromList devices)

  -- Initial list of available commands = all commands enabled by
  -- devices in inventory or installed; and commands that require no
  -- capability.
  allCapabilities r =
    inventoryCapabilities (r ^. installedDevices)
      <> inventoryCapabilities (r ^. robotInventory)
  initialCaps = mconcat $ map allCapabilities robotList
  initialCommands =
    filter
      (maybe True (`S.member` initialCaps) . constCaps)
      allConst

  (genRobots, wf) = buildWorld em (scenario ^. scenarioWorld)
  theWorld = W.newWorld . wf
  theWinCondition = maybe NoWinCondition WinConditions (NE.nonEmpty (scenario ^. scenarioObjectives))
  initGensym = length robotList - 1
  addRecipesWith f gRs = IM.unionWith (<>) (f $ scenario ^. scenarioRecipes) (g ^. gRs)

-- | Take a world description, parsed from a scenario file, and turn
--   it into a list of located robots and a world function.
buildWorld :: EntityMap -> WorldDescription -> ([TRobot], Seed -> WorldFun Int Entity)
buildWorld em WorldDescription {..} = (robots, first fromEnum . wf)
 where
  rs = fromIntegral $ length area
  cs = fromIntegral $ length (head area)
  Coords (ulr, ulc) = locToCoords ul

  worldGrid :: [[(TerrainType, Maybe Entity)]]
  worldGrid = (map . map) (cellTerrain &&& cellEntity) area

  worldArray :: Array (Int64, Int64) (TerrainType, Maybe Entity)
  worldArray = listArray ((ulr, ulc), (ulr + rs - 1, ulc + cs - 1)) (concat worldGrid)

  wf = case defaultTerrain of
    Nothing ->
      (if offsetOrigin then findGoodOrigin else id) . testWorld2FromArray em worldArray
    Just (Cell t e _) -> const (worldFunFromArray worldArray (t, e))

  -- Get all the robots described in cells and set their locations appropriately
  robots :: [TRobot]
  robots =
    area
      & traversed Control.Lens.<.> traversed %@~ (,) -- add (r,c) indices
      & concat
      & concatMap
        ( \((fromIntegral -> r, fromIntegral -> c), Cell _ _ robotList) ->
            let robotWithLoc = trobotLocation ?~ W.coordsToLoc (Coords (ulr + r, ulc + c))
             in map robotWithLoc robotList
        )

-- | Create an initial game state for a specific scenario.
initGameStateForScenario :: String -> Maybe Seed -> Maybe String -> ExceptT Text IO GameState
initGameStateForScenario sceneName userSeed toRun = do
  g <- initGameState
  (scene, path) <- loadScenario sceneName (g ^. entityMap)
  gs <- liftIO $ scenarioToGameState scene userSeed toRun g
  normalPath <- liftIO $ normalizeScenarioPath (gs ^. scenarios) path
  t <- liftIO getZonedTime
  return $
    gs
      & currentScenarioPath ?~ normalPath
      & scenarios . scenarioItemByPath normalPath . _SISingle . _2 . scenarioStatus .~ InProgress t 0 0

-- | For convenience, the 'GameState' corresponding to the classic
--   game with seed 0.
classicGame0 :: ExceptT Text IO GameState
classicGame0 = initGameStateForScenario "classic" (Just 0) Nothing
