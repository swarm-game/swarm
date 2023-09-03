{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.UI (
  UIState (..),
  GoalDisplay (..),
  uiMenu,
  uiPlaying,
  uiCheatMode,
  uiFocusRing,
  uiLaunchConfig,
  uiWorldCursor,
  uiWorldEditor,
  uiREPL,
  uiInventory,
  uiInventorySort,
  uiInventorySearch,
  uiScrollToEnd,
  uiError,
  uiModal,
  uiGoal,
  uiHideGoals,
  uiAchievements,
  lgTicksPerSecond,
  lastFrameTime,
  accumulatedTime,
  tickCount,
  frameCount,
  frameTickCount,
  lastInfoTime,
  uiShowFPS,
  uiShowREPL,
  uiShowZero,
  uiShowDebug,
  uiShowRobots,
  uiHideRobotsUntil,
  uiInventoryShouldUpdate,
  uiTPF,
  uiFPS,
  uiAttrMap,
  scenarioRef,

  -- ** Initialization
  initFocusRing,
  defaultInitLgTicksPerSecond,
  initUIState,
) where

import Brick (AttrMap)
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Arrow ((&&&))
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Lens hiding (from, (<.>))
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.ResourceLoading (getSwarmHistoryPath)
import Swarm.Game.ScenarioInfo (
  ScenarioInfoPair,
 )
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Attr (swarmAttrMap)
import Swarm.TUI.Editor.Model
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.Util
import Swarm.Util.Lens (makeLensesExcluding)
import System.Clock

------------------------------------------------------------
-- UI state
------------------------------------------------------------

-- | The main record holding the UI state.  For access to the fields,
-- see the lenses below.
data UIState = UIState
  { _uiMenu :: Menu
  , _uiPlaying :: Bool
  , _uiCheatMode :: Bool
  , _uiFocusRing :: FocusRing Name
  , _uiLaunchConfig :: LaunchOptions
  , _uiWorldCursor :: Maybe (Cosmic W.Coords)
  , _uiWorldEditor :: WorldEditor Name
  , _uiREPL :: REPLState
  , _uiInventory :: Maybe (Int, BL.List Name InventoryListEntry)
  , _uiInventorySort :: InventorySortOptions
  , _uiInventorySearch :: Maybe Text
  , _uiScrollToEnd :: Bool
  , _uiError :: Maybe Text
  , _uiModal :: Maybe Modal
  , _uiGoal :: GoalDisplay
  , _uiHideGoals :: Bool
  , _uiAchievements :: Map CategorizedAchievement Attainment
  , _uiShowFPS :: Bool
  , _uiShowREPL :: Bool
  , _uiShowZero :: Bool
  , _uiShowDebug :: Bool
  , _uiHideRobotsUntil :: TimeSpec
  , _uiInventoryShouldUpdate :: Bool
  , _uiTPF :: Double
  , _uiFPS :: Double
  , _lgTicksPerSecond :: Int
  , _tickCount :: Int
  , _frameCount :: Int
  , _frameTickCount :: Int
  , _lastFrameTime :: TimeSpec
  , _accumulatedTime :: TimeSpec
  , _lastInfoTime :: TimeSpec
  , _uiAttrMap :: AttrMap
  , _scenarioRef :: Maybe ScenarioInfoPair
  }

--------------------------------------------------
-- Lenses for UIState

makeLensesExcluding ['_lgTicksPerSecond] ''UIState

-- | The current menu state.
uiMenu :: Lens' UIState Menu

-- | Are we currently playing the game?  True = we are playing, and
--   should thus display a world, REPL, etc.; False = we should
--   display the current menu.
uiPlaying :: Lens' UIState Bool

-- | Cheat mode, i.e. are we allowed to turn creative mode on and off?
uiCheatMode :: Lens' UIState Bool

-- | Configuration modal when launching a scenario
uiLaunchConfig :: Lens' UIState LaunchOptions

-- | The focus ring is the set of UI panels we can cycle among using
--   the Tab key.
uiFocusRing :: Lens' UIState (FocusRing Name)

-- | The last clicked position on the world view.
uiWorldCursor :: Lens' UIState (Maybe (Cosmic W.Coords))

-- | State of all World Editor widgets
uiWorldEditor :: Lens' UIState (WorldEditor Name)

-- | The state of REPL panel.
uiREPL :: Lens' UIState REPLState

-- | The order and direction of sorting inventory list.
uiInventorySort :: Lens' UIState InventorySortOptions

-- | The current search string used to narrow the inventory view.
uiInventorySearch :: Lens' UIState (Maybe Text)

-- | The hash value of the focused robot entity (so we can tell if its
--   inventory changed) along with a list of the items in the
--   focused robot's inventory.
uiInventory :: Lens' UIState (Maybe (Int, BL.List Name InventoryListEntry))

-- | A flag telling the UI to scroll the info panel to the very end
--   (used when a new log message is appended).
uiScrollToEnd :: Lens' UIState Bool

-- | When this is @Just@, it represents a popup box containing an
--   error message that is shown on top of the rest of the UI.
uiError :: Lens' UIState (Maybe Text)

-- | When this is @Just@, it represents a modal to be displayed on
--   top of the UI, e.g. for the Help screen.
uiModal :: Lens' UIState (Maybe Modal)

-- | Status of the scenario goal: whether there is one, and whether it
--   has been displayed to the user initially.
uiGoal :: Lens' UIState GoalDisplay

-- | When running with --autoplay, suppress the goal dialogs.
--
-- For developement, the --cheat flag shows goals again.
uiHideGoals :: Lens' UIState Bool

-- | Map of achievements that were attained
uiAchievements :: Lens' UIState (Map CategorizedAchievement Attainment)

-- | A toggle to show the FPS by pressing `f`
uiShowFPS :: Lens' UIState Bool

-- | A toggle to expand or collapse the REPL by pressing `Ctrl-k`
uiShowREPL :: Lens' UIState Bool

-- | A toggle to show or hide inventory items with count 0 by pressing `0`
uiShowZero :: Lens' UIState Bool

-- | A toggle to show debug.
--
-- TODO: #1112 use record for selection of debug features?
uiShowDebug :: Lens' UIState Bool

-- | Hide robots on the world map.
uiHideRobotsUntil :: Lens' UIState TimeSpec

-- | Whether to show or hide robots on the world map.
uiShowRobots :: Getter UIState Bool
uiShowRobots = to (\ui -> ui ^. lastFrameTime > ui ^. uiHideRobotsUntil)

-- | Whether the Inventory ui panel should update
uiInventoryShouldUpdate :: Lens' UIState Bool

-- | Computed ticks per milli seconds
uiTPF :: Lens' UIState Double

-- | Computed frames per milli seconds
uiFPS :: Lens' UIState Double

-- | Attribute map
uiAttrMap :: Lens' UIState AttrMap

-- | The currently active Scenario description, useful for starting over.
scenarioRef :: Lens' UIState (Maybe ScenarioInfoPair)

-- | The base-2 logarithm of the current game speed in ticks/second.
--   Note that we cap this value to the range of +/- log2 INTMAX.
lgTicksPerSecond :: Lens' UIState Int
lgTicksPerSecond = lens _lgTicksPerSecond safeSetLgTicks
 where
  maxLog = finiteBitSize (maxBound :: Int)
  maxTicks = maxLog - 2
  minTicks = 2 - maxLog
  safeSetLgTicks ui lTicks
    | lTicks < minTicks = setLgTicks ui minTicks
    | lTicks > maxTicks = setLgTicks ui maxTicks
    | otherwise = setLgTicks ui lTicks
  setLgTicks ui lTicks = ui {_lgTicksPerSecond = lTicks}

-- | A counter used to track how many ticks have happened since the
--   last time we updated the ticks/frame statistics.
tickCount :: Lens' UIState Int

-- | A counter used to track how many frames have been rendered since the
--   last time we updated the ticks/frame statistics.
frameCount :: Lens' UIState Int

-- | A counter used to track how many ticks have happened in the
--   current frame, so we can stop when we get to the tick cap.
frameTickCount :: Lens' UIState Int

-- | The time of the last info widget update
lastInfoTime :: Lens' UIState TimeSpec

-- | The time of the last 'Frame' event.
lastFrameTime :: Lens' UIState TimeSpec

-- | The amount of accumulated real time.  Every time we get a 'Frame'
--   event, we accumulate the amount of real time that happened since
--   the last frame, then attempt to take an appropriate number of
--   ticks to "catch up", based on the target tick rate.
--
--   See https://gafferongames.com/post/fix_your_timestep/ .
accumulatedTime :: Lens' UIState TimeSpec

--------------------------------------------------
-- UIState initialization

-- | The initial state of the focus ring.
-- NOTE: Normally, the Tab key might cycle through the members of the
-- focus ring. However, the REPL already uses Tab. So, to is not used
-- at all right now for navigating the toplevel focus ring.
initFocusRing :: FocusRing Name
initFocusRing = focusRing $ map FocusablePanel listEnums

-- | The initial tick speed.
defaultInitLgTicksPerSecond :: Int
defaultInitLgTicksPerSecond = 4 -- 2^4 = 16 ticks / second

-- | Initialize the UI state.  This needs to be in the IO monad since
--   it involves reading a REPL history file, getting the current
--   time, and loading text files from the data directory.  The @Bool@
--   parameter indicates whether we should start off by showing the
--   main menu.
initUIState ::
  ( Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  Int ->
  Bool ->
  Bool ->
  m UIState
initUIState speedFactor showMainMenu cheatMode = do
  historyT <- sendIO $ readFileMayT =<< getSwarmHistoryPath False
  let history = maybe [] (map REPLEntry . T.lines) historyT
  startTime <- sendIO $ getTime Monotonic
  achievements <- loadAchievementsInfo
  launchConfigPanel <- sendIO initConfigPanel
  let out =
        UIState
          { _uiMenu = if showMainMenu then MainMenu (mainMenu NewGame) else NoMenu
          , _uiPlaying = not showMainMenu
          , _uiCheatMode = cheatMode
          , _uiLaunchConfig = launchConfigPanel
          , _uiFocusRing = initFocusRing
          , _uiWorldCursor = Nothing
          , _uiWorldEditor = initialWorldEditor startTime
          , _uiREPL = initREPLState $ newREPLHistory history
          , _uiInventory = Nothing
          , _uiInventorySort = defaultSortOptions
          , _uiInventorySearch = Nothing
          , _uiScrollToEnd = False
          , _uiError = Nothing
          , _uiModal = Nothing
          , _uiGoal = emptyGoalDisplay
          , _uiHideGoals = False
          , _uiAchievements = M.fromList $ map (view achievement &&& id) achievements
          , _uiShowFPS = False
          , _uiShowREPL = True
          , _uiShowZero = True
          , _uiShowDebug = False
          , _uiHideRobotsUntil = startTime - 1
          , _uiInventoryShouldUpdate = False
          , _uiTPF = 0
          , _uiFPS = 0
          , _lgTicksPerSecond = speedFactor
          , _lastFrameTime = startTime
          , _accumulatedTime = 0
          , _lastInfoTime = 0
          , _tickCount = 0
          , _frameCount = 0
          , _frameTickCount = 0
          , _uiAttrMap = swarmAttrMap
          , _scenarioRef = Nothing
          }
  return out
