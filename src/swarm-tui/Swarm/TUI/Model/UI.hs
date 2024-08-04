{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.UI (
  UIState (..),
  UIGameplay (..),
  UITiming (..),
  UIInventory (..),
  GoalDisplay (..),
  uiGameplay,
  uiPopups,
  uiTiming,
  uiInventory,
  uiMenu,
  uiPlaying,
  uiDebugOptions,
  uiFocusRing,
  uiLaunchConfig,
  uiWorldCursor,
  uiWorldEditor,
  uiREPL,
  uiInventoryList,
  uiInventorySort,
  uiInventorySearch,
  uiScrollToEnd,
  uiModal,
  uiGoal,
  uiStructure,
  uiIsAutoPlay,
  uiHideObjectives,
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
import Data.List.Extra (enumerate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set (Set)
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
import Swarm.Game.World.Coords
import Swarm.TUI.Editor.Model
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep
import Swarm.TUI.Model.DebugOption (DebugOption)
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Popup
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.Structure
import Swarm.TUI.View.Attribute.Attr (swarmAttrMap)
import Swarm.Util
import Swarm.Util.Lens (makeLensesExcluding, makeLensesNoSigs)
import System.Clock

data UITiming = UITiming
  { _uiShowFPS :: Bool
  , _uiTPF :: Double
  , _uiFPS :: Double
  , _lgTicksPerSecond :: Int
  , _tickCount :: Int
  , _frameCount :: Int
  , _frameTickCount :: Int
  , _lastFrameTime :: TimeSpec
  , _accumulatedTime :: TimeSpec
  , _lastInfoTime :: TimeSpec
  }

-- * Lenses for UITiming

makeLensesExcluding ['_lgTicksPerSecond] ''UITiming

-- | A toggle to show the FPS by pressing @f@
uiShowFPS :: Lens' UITiming Bool

-- | Computed ticks per milliseconds
uiTPF :: Lens' UITiming Double

-- | Computed frames per milliseconds
uiFPS :: Lens' UITiming Double

-- | The base-2 logarithm of the current game speed in ticks/second.
--   Note that we cap this value to the range of +/- log2 INTMAX.
lgTicksPerSecond :: Lens' UITiming Int
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
tickCount :: Lens' UITiming Int

-- | A counter used to track how many frames have been rendered since the
--   last time we updated the ticks/frame statistics.
frameCount :: Lens' UITiming Int

-- | A counter used to track how many ticks have happened in the
--   current frame, so we can stop when we get to the tick cap.
frameTickCount :: Lens' UITiming Int

-- | The time of the last info widget update
lastInfoTime :: Lens' UITiming TimeSpec

-- | The time of the last 'Swarm.TUI.Model.Frame' event.
lastFrameTime :: Lens' UITiming TimeSpec

-- | The amount of accumulated real time.  Every time we get a 'Swarm.TUI.Model.Frame'
--   event, we accumulate the amount of real time that happened since
--   the last frame, then attempt to take an appropriate number of
--   ticks to "catch up", based on the target tick rate.
--
--   See https://gafferongames.com/post/fix_your_timestep/ .
accumulatedTime :: Lens' UITiming TimeSpec

data UIInventory = UIInventory
  { _uiInventoryList :: Maybe (Int, BL.List Name InventoryListEntry)
  , _uiInventorySort :: InventorySortOptions
  , _uiInventorySearch :: Maybe Text
  , _uiShowZero :: Bool
  , _uiInventoryShouldUpdate :: Bool
  }

-- * Lenses for UIInventory

makeLensesNoSigs ''UIInventory

-- | The order and direction of sorting inventory list.
uiInventorySort :: Lens' UIInventory InventorySortOptions

-- | The current search string used to narrow the inventory view.
uiInventorySearch :: Lens' UIInventory (Maybe Text)

-- | The hash value of the focused robot entity (so we can tell if its
--   inventory changed) along with a list of the items in the
--   focused robot's inventory.
uiInventoryList :: Lens' UIInventory (Maybe (Int, BL.List Name InventoryListEntry))

-- | A toggle to show or hide inventory items with count 0 by pressing @0@
uiShowZero :: Lens' UIInventory Bool

-- | Whether the Inventory ui panel should update
uiInventoryShouldUpdate :: Lens' UIInventory Bool

-- | The main record holding the gameplay UI state.  For access to the fields,
-- see the lenses below.
data UIGameplay = UIGameplay
  { _uiFocusRing :: FocusRing Name
  , _uiWorldCursor :: Maybe (Cosmic Coords)
  , _uiWorldEditor :: WorldEditor Name
  , _uiREPL :: REPLState
  , _uiInventory :: UIInventory
  , _uiScrollToEnd :: Bool
  , _uiModal :: Maybe Modal
  , _uiGoal :: GoalDisplay
  , _uiStructure :: StructureDisplay
  , _uiIsAutoPlay :: Bool
  , _uiHideObjectives :: Bool
  , _uiShowREPL :: Bool
  , _uiShowDebug :: Bool
  , _uiHideRobotsUntil :: TimeSpec
  , _uiTiming :: UITiming
  , _scenarioRef :: Maybe ScenarioInfoPair
  }

-- * Lenses for UIGameplay

makeLensesNoSigs ''UIGameplay

-- | Temporal information for gameplay UI
uiTiming :: Lens' UIGameplay UITiming

-- | Inventory information for gameplay UI
uiInventory :: Lens' UIGameplay UIInventory

-- | The focus ring is the set of UI panels we can cycle among using
--   the @Tab@ key.
uiFocusRing :: Lens' UIGameplay (FocusRing Name)

-- | The last clicked position on the world view.
uiWorldCursor :: Lens' UIGameplay (Maybe (Cosmic Coords))

-- | State of all World Editor widgets
uiWorldEditor :: Lens' UIGameplay (WorldEditor Name)

-- | The state of REPL panel.
uiREPL :: Lens' UIGameplay REPLState

-- | A flag telling the UI to scroll the info panel to the very end
--   (used when a new log message is appended).
uiScrollToEnd :: Lens' UIGameplay Bool

-- | When this is 'Just', it represents a modal to be displayed on
--   top of the UI, e.g. for the Help screen.
uiModal :: Lens' UIGameplay (Maybe Modal)

-- | Status of the scenario goal: whether there is one, and whether it
--   has been displayed to the user initially.
uiGoal :: Lens' UIGameplay GoalDisplay

-- | Definition and status of a recognizable structure
uiStructure :: Lens' UIGameplay StructureDisplay

-- | When running with @--autoplay@ the progress will not be saved.
uiIsAutoPlay :: Lens' UIGameplay Bool

-- | Do not open objectives modals on objective completion.
uiHideObjectives :: Lens' UIGameplay Bool

-- | A toggle to expand or collapse the REPL by pressing @Ctrl-k@
uiShowREPL :: Lens' UIGameplay Bool

-- | A toggle to show CESK machine debug view and step through it.
--
-- Note that the ability to use it can be enabled by player robot
-- gaining the capability, or being in creative mode or with
-- the debug option 'Swarm.TUI.Model.DebugOption.DebugCESK'.
uiShowDebug :: Lens' UIGameplay Bool

-- | Hide robots on the world map.
uiHideRobotsUntil :: Lens' UIGameplay TimeSpec

-- | Whether to show or hide robots on the world map.
uiShowRobots :: Getter UIGameplay Bool
uiShowRobots = to (\ui -> ui ^. uiTiming . lastFrameTime > ui ^. uiHideRobotsUntil)

-- | The currently active Scenario description, useful for starting over.
scenarioRef :: Lens' UIGameplay (Maybe ScenarioInfoPair)

-- * Toplevel UIState definition

data UIState = UIState
  { _uiMenu :: Menu
  , _uiPlaying :: Bool
  , _uiDebugOptions :: Set DebugOption
  , _uiLaunchConfig :: LaunchOptions
  , _uiAchievements :: Map CategorizedAchievement Attainment
  , _uiAttrMap :: AttrMap
  , _uiGameplay :: UIGameplay
  , _uiPopups :: PopupState
  }

-- * Lenses for UIState

makeLensesNoSigs ''UIState

-- | The current menu state.
uiMenu :: Lens' UIState Menu

-- | Are we currently playing the game?
--
-- * 'True' = we are playing, and
--   should thus display a world, REPL, etc.
-- * False = we should
--   display the current menu.
uiPlaying :: Lens' UIState Bool

-- | Debugging features, for example are we allowed to turn creative mode on and off?
uiDebugOptions :: Lens' UIState (Set DebugOption)

-- | Configuration modal when launching a scenario
uiLaunchConfig :: Lens' UIState LaunchOptions

-- | Map of achievements that were attained
uiAchievements :: Lens' UIState (Map CategorizedAchievement Attainment)

-- | Attribute map
uiAttrMap :: Lens' UIState AttrMap

-- | UI active during live gameplay
uiGameplay :: Lens' UIState UIGameplay

-- | Queue of popups to display
uiPopups :: Lens' UIState PopupState

-- * UIState initialization

-- | The initial state of the focus ring.
-- NOTE: Normally, the Tab key might cycle through the members of the
-- focus ring. However, the REPL already uses Tab. So, to is not used
-- at all right now for navigating the toplevel focus ring.
initFocusRing :: FocusRing Name
initFocusRing = focusRing $ map FocusablePanel enumerate

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
  Set DebugOption ->
  m UIState
initUIState speedFactor showMainMenu debug = do
  -- TODO: ondra - add ui config for silence
  historyT <- sendIO $ readFileMayT =<< getSwarmHistoryPath False
  let history = maybe [] (map mkREPLSubmission . T.lines) historyT
  startTime <- sendIO $ getTime Monotonic
  achievements <- loadAchievementsInfo
  launchConfigPanel <- sendIO initConfigPanel
  return
    UIState
      { _uiMenu = if showMainMenu then MainMenu (mainMenu NewGame) else NoMenu
      , _uiPlaying = not showMainMenu
      , _uiDebugOptions = debug
      , _uiLaunchConfig = launchConfigPanel
      , _uiAchievements = M.fromList $ map (view achievement &&& id) achievements
      , _uiAttrMap = swarmAttrMap
      , _uiPopups = initPopupState
      , _uiGameplay =
          UIGameplay
            { _uiFocusRing = initFocusRing
            , _uiWorldCursor = Nothing
            , _uiWorldEditor = initialWorldEditor startTime
            , _uiREPL = initREPLState $ newREPLHistory history
            , _uiInventory =
                UIInventory
                  { _uiInventoryList = Nothing
                  , _uiInventorySort = defaultSortOptions
                  , _uiInventorySearch = Nothing
                  , _uiShowZero = True
                  , _uiInventoryShouldUpdate = False
                  }
            , _uiScrollToEnd = False
            , _uiModal = Nothing
            , _uiGoal = emptyGoalDisplay
            , _uiStructure = emptyStructureDisplay
            , _uiIsAutoPlay = False
            , _uiHideObjectives = True
            , _uiTiming =
                UITiming
                  { _uiShowFPS = False
                  , _uiTPF = 0
                  , _uiFPS = 0
                  , _lgTicksPerSecond = speedFactor
                  , _lastFrameTime = startTime
                  , _accumulatedTime = 0
                  , _lastInfoTime = 0
                  , _tickCount = 0
                  , _frameCount = 0
                  , _frameTickCount = 0
                  }
            , _uiShowREPL = True
            , _uiShowDebug = False
            , _uiHideRobotsUntil = startTime - 1
            , _scenarioRef = Nothing
            }
      }
