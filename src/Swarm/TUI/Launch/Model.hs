{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.TUI.Launch.Model where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens (makeLenses)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Swarm.Game.ScenarioInfo
import Swarm.Game.State (CodeToRun)
import Swarm.Game.WorldGen (Seed)
import Swarm.TUI.Model.Name

data LaunchParms a = LaunchParms
  { seedVal :: a (Maybe Seed)
  , initialCode :: a (Maybe CodeToRun)
  }

-- | Use this to store error messages
-- on individual fields
type EditingLaunchParms = LaunchParms (Either Text)

-- | In this stage in the UI pipeline, both fields
-- have already been validated, and "Nothing" means
-- that the field is simply absent.
type ValidatedLaunchParms = LaunchParms Identity

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

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions
  { _controls :: LaunchControls
  , _editingParams :: EditingLaunchParms
  }

makeLenses ''LaunchOptions
