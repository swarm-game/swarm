{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.UI.Gameplay where

import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (<.>))
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Text (Text)
import Swarm.Game.ScenarioInfo (
  ScenarioInfoPair,
 )
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Editor.Model
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Model.Dialog.Goal
import Swarm.TUI.Model.Dialog.Structure
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.View.Robot.Type
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

-- | State that backs various modal dialogs
data UIDialogs = UIDialogs
  { _uiModal :: Maybe Modal
  , _uiGoal :: GoalDisplay
  , _uiStructure :: StructureDisplay
  , _uiRobot :: RobotDisplay
  }

-- * Lenses for UIDialogs

makeLensesNoSigs ''UIDialogs

-- | When this is 'Just', it represents a modal to be displayed on
--   top of the UI, e.g. for the Help screen.
uiModal :: Lens' UIDialogs (Maybe Modal)

-- | Status of the scenario goal: whether there is one, and whether it
--   has been displayed to the user initially.
uiGoal :: Lens' UIDialogs GoalDisplay

-- | Definition and status of a recognizable structure
uiStructure :: Lens' UIDialogs StructureDisplay

-- | Definition and status of a recognizable structure
uiRobot :: Lens' UIDialogs RobotDisplay

-- | The main record holding the gameplay UI state.  For access to the fields,
-- see the lenses below.
data UIGameplay = UIGameplay
  { _uiFocusRing :: FocusRing Name
  , _uiWorldCursor :: Maybe (Cosmic Coords)
  , _uiWorldEditor :: WorldEditor Name
  , _uiREPL :: REPLState
  , _uiInventory :: UIInventory
  , _uiScrollToEnd :: Bool
  , _uiDialogs :: UIDialogs
  , _uiIsAutoPlay :: Bool
  , _uiAutoShowObjectives :: Bool
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

-- | State that backs various modal dialogs
uiDialogs :: Lens' UIGameplay UIDialogs

-- | When running with @--autoplay@ the progress will not be saved.
uiIsAutoPlay :: Lens' UIGameplay Bool

-- | Do not open objectives modals on objective completion.
uiAutoShowObjectives :: Lens' UIGameplay Bool

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
