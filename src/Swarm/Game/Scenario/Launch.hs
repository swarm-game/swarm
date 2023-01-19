{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Scenario.Launch where

import Brick.Focus qualified as Focus
import Brick.Forms qualified as Forms
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens (makeLenses)
import Swarm.TUI.Model.Name

newtype SeedSelection = SeedSelection
  { _seedVal :: Int
  }

makeLenses ''SeedSelection

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions
  { fileBrowser :: Maybe (FB.FileBrowser Name)
  , seedSelectionForm :: Forms.Form SeedSelection () Name
  , scenarioConfigFocusRing :: Focus.FocusRing Name
  }
