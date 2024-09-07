{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for presentation of Robot details.
module Swarm.TUI.View.Robot where

import Brick hiding (Direction, Location)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Table qualified as BT
import Control.Lens hiding (from, (<.>))
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Linear
import Numeric (showFFloat)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Entity as E
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Robot.Activity
import Swarm.Game.Robot.Concrete
import Swarm.Game.State
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Tick (addTicks)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Model.DebugOption (DebugOption (..))
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.Util.UnitInterval
import Swarm.Util.WindowedCounter qualified as WC

-- | Render the percentage of ticks that this robot was active.
-- This indicator can take some time to "warm up" and stabilize
-- due to the sliding window.
--
-- == Use of previous tick
-- The 'Swarm.Game.Step.gameTick' function runs all robots, then increments the current tick.
-- So at the time we are rendering a frame, the current tick will always be
-- strictly greater than any ticks stored in the 'WC.WindowedCounter' for any robot;
-- hence 'WC.getOccupancy' will never be @1@ if we use the current tick directly as
-- obtained from the 'ticks' function.
-- So we "rewind" it to the previous tick for the purpose of this display.
renderDutyCycle :: TemporalState -> Robot -> Widget Name
renderDutyCycle temporalState robot =
  withAttr dutyCycleAttr . str . flip (showFFloat (Just 1)) "%" $ dutyCyclePercentage
 where
  curTicks = temporalState ^. ticks
  window = robot ^. activityCounts . activityWindow

  -- Rewind to previous tick
  latestRobotTick = addTicks (-1) curTicks
  dutyCycleRatio = WC.getOccupancy latestRobotTick window

  dutyCycleAttr = safeIndex dutyCycleRatio meterAttributeNames

  dutyCyclePercentage :: Double
  dutyCyclePercentage = 100 * getValue dutyCycleRatio

-- robotsListWidget :: AppState -> Widget Name
-- robotsListWidget s = hCenter table
--  where
--   table =
--     BT.renderTable
--       . BT.columnBorders False
--       . BT.setDefaultColAlignment BT.AlignCenter
--       -- Inventory count is right aligned
--       . BT.alignRight 4
--       $ robotsTable c

--   c = RobotRenderingContext {
--       _mygs = s ^. gameState
--     , _gameplay = s ^. uiState . uiGameplay
--     , _timing = s ^. uiState . uiGameplay . uiTiming
--     , _uiDbg = s ^. uiState . uiDebugOptions
--     }
