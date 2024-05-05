{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for Structure presentation.
module Swarm.TUI.Model.Structure where

import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens (makeLenses)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario (Cell)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)

data StructureDisplay = StructureDisplay
  { _structurePanelListWidget :: BL.List Name (StructureInfo Cell Entity)
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  , _structurePanelFocus :: FocusRing Name
  }

makeLenses ''StructureDisplay

emptyStructureDisplay :: StructureDisplay
emptyStructureDisplay =
  StructureDisplay
    (BL.list (StructureWidgets StructuresList) mempty 1)
    (focusRing $ map StructureWidgets listEnums)
