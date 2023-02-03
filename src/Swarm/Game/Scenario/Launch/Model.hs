{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Scenario.Launch.Model where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens (makeLenses)
import Data.Text (Text)
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)

newtype SeedSelection = SeedSelection
  { _seedVal :: Int
  }

makeLenses ''SeedSelection

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions
  { _fileBrowser :: Maybe (FB.FileBrowser Name)
  , _seedValueEditor :: Editor Text Name
  , _scenarioConfigFocusRing :: Focus.FocusRing Name
  , _isDisplayedFor :: Maybe ScenarioInfoPair
  }

makeLenses ''LaunchOptions

initConfigPanel :: LaunchOptions
initConfigPanel =
  LaunchOptions Nothing myForm ring Nothing
 where
  myForm =
    editorText
      (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
      (Just 1)
      "0"
  ring = Focus.focusRing $ map (ScenarioConfigControl . ScenarioConfigPanelControl) listEnums
