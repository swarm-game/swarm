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

data FileBrowserControl = FileBrowserControl
  { _fbWidget :: FB.FileBrowser Name
  , _fbIsDisplayed :: Bool
  }

makeLenses ''FileBrowserControl

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions
  { _fileBrowser :: FileBrowserControl
  , _seedValueEditor :: Editor Text Name
  , _scenarioConfigFocusRing :: Focus.FocusRing Name
  , _isDisplayedFor :: Maybe ScenarioInfoPair
  }

makeLenses ''LaunchOptions

initConfigPanel :: IO LaunchOptions
initConfigPanel = do
  fb <-
    FB.newFileBrowser
      FB.selectNonDirectories
      -- (const False)
      (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
      Nothing
  let configuredFB = FB.setFileBrowserEntryFilter (Just $ FB.fileExtensionMatch "sw") fb
  return $ LaunchOptions (FileBrowserControl configuredFB False) myForm ring Nothing
 where
  myForm =
    editorText
      (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
      (Just 1)
      "0"
  ring = Focus.focusRing $ map (ScenarioConfigControl . ScenarioConfigPanelControl) listEnums
