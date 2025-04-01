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
import Swarm.Failure (SystemFailure)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep
import Swarm.TUI.Model.DebugOption (DebugOption)
import Swarm.TUI.Model.Dialog
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Attribute.Attr (swarmAttrMap)
import Swarm.Util.Lens (makeLensesNoSigs)

-- * Toplevel UIState definition

-- | UI state independent of an actively-playing scenario.
-- Compare to 'UIGameplay', which contains UI state for an
-- active scenario.
data UIState = UIState
  { _uiMenu :: Menu
  , _uiPlaying :: Bool
  , _uiDebugOptions :: Set DebugOption
  , _uiLaunchConfig :: LaunchOptions
  , _uiAchievements :: Map CategorizedAchievement Attainment
  , _uiAttrMap :: AttrMap
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
--   time, and loading text files from the data directory.
initUIState ::
  ( Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  UIInitOptions ->
  m UIState
initUIState UIInitOptions {..} = do
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
      }
