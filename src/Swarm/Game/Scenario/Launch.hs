module Swarm.Game.Scenario.Launch where

import Brick.Widgets.FileBrowser qualified as FB
import Brick.Forms qualified as BF
import Swarm.TUI.Model.Name

newtype SeedSelection = SeedSelection Int

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions {
    fileBrowser :: Maybe (FB.FileBrowser Name)
  , seedSelectionForm :: BF.Form SeedSelection () Name
  }

