{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Robot.Type where

import Brick.Focus (FocusRing)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.TabularList.Grid
import Control.Lens hiding (Const, from, (<.>))
import Data.Sequence (Seq)
import Swarm.Game.Robot
import Swarm.Language.Syntax (Const)
import Swarm.Log
import Swarm.TUI.Model.Name

data RobotDetailsPaneState = RobotDetailsPaneState
  { _detailFocus :: FocusRing Name
  , _logsList :: BL.GenericList Name Seq LogEntry
  , _cmdHistogramList :: BL.List Name (Const, Int)
  }

makeLenses ''RobotDetailsPaneState

data RobotDisplay = RobotDisplay
  { _isDetailsOpened :: Bool
  , _robotsGridList :: GridTabularList Name RID
  , _robotDetailsPaneState :: RobotDetailsPaneState
  }

makeLenses ''RobotDisplay
