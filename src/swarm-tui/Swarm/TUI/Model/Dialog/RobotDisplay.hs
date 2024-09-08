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

data WidthWidget = WidthWidget
  { wWidth :: Int
  , wWidget :: Widget Name
  }

newtype Widths = Widths
  { robotRowWidths :: [ColWidth]
  }
  deriving (Generic)

type RobotWidgetRow = LibRobotRow WidthWidget
type RobotHeaderRow = LibRobotRow String

data LibRobotRow a = LibRobotRow
  { _fID :: a
  , _fName :: a
  , _fAge :: a
  , _fPos :: a
  , _fItems :: a
  , _fStatus :: a
  , _fActns :: a
  , _fCmds :: a
  , _fCycles :: a
  , _fActivity :: a
  , _fLog :: a
  }
  deriving (Functor)

data RobotsDisplayMode = RobotList | SingleRobotDetails
  deriving (Eq, Show, Enum, Bounded)

type LibraryList = MixedTabularList Name RobotWidgetRow Widths
type LibraryRenderers = MixedRenderers Name RobotWidgetRow Widths

data RobotDisplay = RobotDisplay
  { _robotsDisplayMode :: RobotsDisplayMode
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  , _lastFocusedRobotId :: Maybe RID
  , _libList :: LibraryList
  , _libRenderers :: LibraryRenderers
  }

makeLenses ''RobotDisplay
