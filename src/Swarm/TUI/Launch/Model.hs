{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.TUI.Launch.Model where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens (makeLenses)
import Data.Text (Text)
import Swarm.Game.ScenarioInfo
import Swarm.Game.State (CodeToRun)
import Swarm.Game.WorldGen (Seed)
import Swarm.TUI.Model.Name

data ValidatedLaunchParms = ValidatedLaunchParms
  { seedVal :: Maybe Seed
  , initialCode :: Maybe CodeToRun
  }

data FileBrowserControl = FileBrowserControl
  { _fbWidget :: FB.FileBrowser Name
  , _fbIsDisplayed :: Bool
  }

makeLenses ''FileBrowserControl

-- | UI elements to configure scenario launch options
data LaunchControls = LaunchControls
  { _fileBrowser :: FileBrowserControl
  , _seedValueEditor :: Editor Text Name
  , _scenarioConfigFocusRing :: Focus.FocusRing Name
  , _isDisplayedFor :: Maybe ScenarioInfoPair
  }

makeLenses ''LaunchControls

data LaunchFormError = LaunchFormError
  { widget :: ScenarioConfigPanelFocusable
  , message :: Text
  }

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions
  { _controls :: LaunchControls
  , _validatedParams :: Either LaunchFormError ValidatedLaunchParms
  }

makeLenses ''LaunchOptions
