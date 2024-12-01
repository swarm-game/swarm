{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for Structure presentation.
module Swarm.TUI.Model.Dialog.Structure where

import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens (makeLenses)
import Data.List.Extra (enumerate)
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.TUI.Model.Name

data StructureDisplay a = StructureDisplay
  { _structurePanelListWidget :: BL.List Name (StructureInfo (NamedArea a) Entity)
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  , _structurePanelFocus :: FocusRing Name
  }

makeLenses ''StructureDisplay

emptyStructureDisplay :: StructureDisplay a
emptyStructureDisplay =
  StructureDisplay
    (BL.list (StructureWidgets StructuresList) mempty 1)
    (focusRing $ map StructureWidgets enumerate)
