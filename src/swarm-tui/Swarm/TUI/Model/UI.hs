{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.UI (
  UIState (..),
  uiMenu,
  uiPlaying,
  uiDebugOptions,
  uiLaunchConfig,
  uiGameplay,
  uiAchievements,
  uiAttrMap,
  uiPopups,

  -- ** Initialization
  initFocusRing,
  defaultInitLgTicksPerSecond,
  initUIState,
  UIInitOptions (..),
) where

import Brick (AttrMap)
import Brick.Focus
import Control.Arrow ((&&&))
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Lens hiding (from, (<.>))
import Data.List.Extra (enumerate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text qualified as T
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.ResourceLoading (getSwarmHistoryPath)
import Swarm.TUI.Editor.Model
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep
import Swarm.TUI.Model.DebugOption (DebugOption)
import Swarm.TUI.Model.Dialog
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr (swarmAttrMap)
import Swarm.TUI.View.Robot
import Swarm.TUI.View.Robot.Type
import Swarm.Util
import Swarm.Util.Lens (makeLensesNoSigs)
import System.Clock

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

data UIInitOptions = UIInitOptions
  { speed :: Int
  , showMainMenu :: Bool
  , autoShowObjectives :: Bool
  , debugOptions :: Set DebugOption
  }
  deriving (Eq, Show)

-- | Initialize the UI state.  This needs to be in the IO monad since
--   it involves reading a REPL history file, getting the current
--   time, and loading text files from the data directory.  The @Bool@
--   parameter indicates whether we should start off by showing the
--   main menu.
initUIState ::
  ( Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  UIInitOptions ->
  m UIState
initUIState UIInitOptions {..} = do
  historyT <- sendIO $ readFileMayT =<< getSwarmHistoryPath False
  let history = maybe [] (map mkREPLSubmission . T.lines) historyT
  startTime <- sendIO $ getTime Monotonic
  achievements <- loadAchievementsInfo
  launchConfigPanel <- sendIO initConfigPanel
  return
    UIState
      { _uiMenu = if showMainMenu then MainMenu (mainMenu NewGame) else NoMenu
      , _uiPlaying = not showMainMenu
      , _uiDebugOptions = debugOptions
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
            , _uiDialogs =
                UIDialogs
                  { _uiModal = Nothing
                  , _uiGoal = emptyGoalDisplay
                  , _uiStructure = emptyStructureDisplay
                  , _uiRobot =
                      RobotDisplay
                        { _robotDetailsFocus = focusRing $ map (RobotsListDialog . SingleRobotDetails) enumerate
                        , _isDetailsOpened = False
                        , _robotListContent = emptyRobotDisplay debugOptions
                        }
                  }
            , _uiIsAutoPlay = False
            , _uiAutoShowObjectives = autoShowObjectives
            , _uiTiming =
                UITiming
                  { _uiShowFPS = False
                  , _uiTPF = 0
                  , _uiFPS = 0
                  , _lgTicksPerSecond = speed
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
