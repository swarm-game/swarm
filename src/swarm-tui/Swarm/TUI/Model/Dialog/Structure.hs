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
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario (RecognizableStructureContent)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Language.Syntax (Phase (..))
import Swarm.TUI.Model.Name

data StructureDisplay = StructureDisplay
  { _structurePanelListWidget :: BL.List Name (StructureInfo Entity (RecognizableStructureContent Typed))
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  , _structurePanelFocus :: FocusRing Name
  }

makeLenses ''StructureDisplay

emptyStructureDisplay :: StructureDisplay
emptyStructureDisplay =
  StructureDisplay
    (BL.list (StructureWidgets StructuresList) mempty 1)
    (focusRing $ map StructureWidgets enumerate)
