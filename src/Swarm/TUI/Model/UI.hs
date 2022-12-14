{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Swarm.TUI.Model.UI (
  UIState (..),
  uiMenu,
  uiPlaying,
  uiCheatMode,
  uiFocusRing,
  uiWorldCursor,
  uiREPL,
  uiInventory,
  uiInventorySort,
  uiMoreInfoTop,
  uiMoreInfoBot,
  uiScrollToEnd,
  uiError,
  uiModal,
  uiGoal,
  lgTicksPerSecond,
  lastFrameTime,
  accumulatedTime,
  tickCount,
  frameCount,
  frameTickCount,
  lastInfoTime,
  uiShowFPS,
  uiShowZero,
  uiShowRobots,
  uiHideRobotsUntil,
  uiInventoryShouldUpdate,
  uiTPF,
  uiFPS,
  scenarioRef,
  appData,
) where

import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (<.>))
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Map (Map)
import Data.Text (Text)
import Swarm.Game.ScenarioInfo (
  ScenarioInfoPair,
 )
import Swarm.Game.World qualified as W
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Model.Menus
import Swarm.TUI.Model.Names
import Swarm.TUI.Model.Repl
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
  , _uiWorldCursor :: Maybe W.Coords
  , _uiREPL :: REPLState
  , _uiInventory :: Maybe (Int, BL.List Name InventoryListEntry)
  , _uiInventorySort :: InventorySortOptions
  , _uiMoreInfoTop :: Bool
  , _uiMoreInfoBot :: Bool
  , _uiScrollToEnd :: Bool
  , _uiError :: Maybe Text
  , _uiModal :: Maybe Modal
  , _uiGoal :: Maybe [Text]
  , _uiShowFPS :: Bool
  , _uiShowZero :: Bool
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
  , _appData :: Map Text Text
  , _scenarioRef :: Maybe ScenarioInfoPair
  }

--------------------------------------------------
-- Lenses for UIState

let exclude = ['_lgTicksPerSecond]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''UIState

-- | The current menu state.
uiMenu :: Lens' UIState Menu

-- | Are we currently playing the game?  True = we are playing, and
--   should thus display a world, REPL, etc.; False = we should
--   display the current menu.
uiPlaying :: Lens' UIState Bool

-- | Cheat mode, i.e. are we allowed to turn creative mode on and off?
uiCheatMode :: Lens' UIState Bool

-- | The focus ring is the set of UI panels we can cycle among using
--   the Tab key.
uiFocusRing :: Lens' UIState (FocusRing Name)

-- | The last clicked position on the world view.
uiWorldCursor :: Lens' UIState (Maybe W.Coords)

-- | The state of REPL panel.
uiREPL :: Lens' UIState REPLState

-- | The order and direction of sorting inventory list.
uiInventorySort :: Lens' UIState InventorySortOptions

-- | The hash value of the focused robot entity (so we can tell if its
--   inventory changed) along with a list of the items in the
--   focused robot's inventory.
uiInventory :: Lens' UIState (Maybe (Int, BL.List Name InventoryListEntry))

-- | Does the info panel contain more content past the top of the panel?
uiMoreInfoTop :: Lens' UIState Bool

-- | Does the info panel contain more content past the bottom of the panel?
uiMoreInfoBot :: Lens' UIState Bool

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
uiGoal :: Lens' UIState (Maybe [Text])

-- | A toggle to show the FPS by pressing `f`
uiShowFPS :: Lens' UIState Bool

-- | A toggle to show or hide inventory items with count 0 by pressing `0`
uiShowZero :: Lens' UIState Bool

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

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' UIState (Map Text Text)
