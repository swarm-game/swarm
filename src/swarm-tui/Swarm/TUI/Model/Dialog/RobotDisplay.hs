{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Dialog.RobotDisplay where

import Brick
import Brick.Widgets.TabularList.Mixed
import Control.Lens hiding (from, (<.>))
import GHC.Generics (Generic)
import Swarm.Game.Robot
import Swarm.TUI.Model.Name

newtype Widths = Widths
  { robotRowWidths :: [ColWidth]
  }
  deriving (Generic)

data LibRobotRow = LibRobotRow
  { _fName :: Widget Name
  , _fAge :: Widget Name
  , _fPos :: Widget Name
  , _fItems :: Widget Name
  , _fStatus :: Widget Name
  , _fActns :: Widget Name
  , _fCmds :: Widget Name
  , _fCycles :: Widget Name
  , _fActivity :: Widget Name
  , _fLog :: Widget Name
  }

data RobotsDisplayMode = RobotList | SingleRobotDetails
  deriving (Eq, Show, Enum, Bounded)

type LibraryList = MixedTabularList Name LibRobotRow Widths
type LibraryRenderers = MixedRenderers Name LibRobotRow Widths

data RobotDisplay = RobotDisplay
  { _robotsDisplayMode :: RobotsDisplayMode
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  , _lastFocusedRobotId :: Maybe RID
  , _libList :: LibraryList
  , _libRenderers :: LibraryRenderers
  }

makeLenses ''RobotDisplay
