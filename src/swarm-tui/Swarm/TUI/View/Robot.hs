{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for presentation of Robot details.
module Swarm.TUI.View.Robot where

import Data.Set (Set)
import Control.Lens hiding (from, (<.>))
import Brick hiding (Direction, Location)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Table qualified as BT
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
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
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (..))
import Swarm.TUI.Model.UI
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Util as VU
import Swarm.Util
import Swarm.Util.UnitInterval
import Swarm.Util.WindowedCounter qualified as WC
import System.Clock (TimeSpec (..))

data RobotRenderingContext = RobotRenderingContext {
    _mygs :: GameState
  , _gameplay :: UIGameplay
  , _timing :: UITiming
  , _uiDbg :: Set DebugOption
  }

makeLenses ''RobotRenderingContext

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

robotsListWidget :: AppState -> Widget Name
robotsListWidget s = hCenter table
 where
  table =
    BT.renderTable
      . BT.columnBorders False
      . BT.setDefaultColAlignment BT.AlignCenter
      -- Inventory count is right aligned
      . BT.alignRight 4
      $ robotsTable c

  c = RobotRenderingContext {
      _mygs = s ^. gameState
    , _gameplay = s ^. uiState . uiGameplay
    , _timing = s ^. uiState . uiGameplay . uiTiming
    , _uiDbg = s ^. uiState . uiDebugOptions
    }


robotsTable :: RobotRenderingContext -> BT.Table Name
robotsTable c =
  BT.table $
    map (padLeftRight 1) <$> (headers : robotRows)
 where
  headings =
    [ "Name"
    , "Age"
    , "Pos"
    , "Items"
    , "Status"
    , "Actns"
    , "Cmds"
    , "Cycles"
    , "Activity"
    , "Log"
    ]
  headers = withAttr robotAttr . txt <$> applyWhen debugRID ("ID" :) headings
  robotRows = mkRobotRow <$> robots
  mkRobotRow robot =
    applyWhen debugRID (idWidget :) cells
   where
    cells =
      [ nameWidget
      , str ageStr
      , locWidget
      , padRight (Pad 1) (str $ show rInvCount)
      , statusWidget
      , str $ show $ robot ^. activityCounts . tangibleCommandCount
      , -- TODO(#1341): May want to expose the details of this histogram in
        -- a per-robot pop-up
        str . show . sum . M.elems $ robot ^. activityCounts . commandsHistogram
      , str $ show $ robot ^. activityCounts . lifetimeStepCount
      , renderDutyCycle (c ^. mygs . temporal) robot
      , txt rLog
      ]

    idWidget = str $ show $ robot ^. robotID
    nameWidget =
      hBox
        [ renderDisplay (robot ^. robotDisplay)
        , highlightSystem . txt $ " " <> robot ^. robotName
        ]

    highlightSystem = if robot ^. systemRobot then withAttr highlightAttr else id

    ageStr
      | age < 60 = show age <> "sec"
      | age < 3600 = show (age `div` 60) <> "min"
      | age < 3600 * 24 = show (age `div` 3600) <> "hour"
      | otherwise = show (age `div` 3600 * 24) <> "day"
     where
      TimeSpec createdAtSec _ = robot ^. robotCreatedAt
      TimeSpec nowSec _ = c ^. timing . lastFrameTime
      age = nowSec - createdAtSec

    rInvCount = sum $ map fst . E.elems $ robot ^. robotEntity . entityInventory
    rLog
      | robot ^. robotLogUpdated = "x"
      | otherwise = " "

    locWidget = hBox [worldCell, str $ " " <> locStr]
     where
      rCoords = fmap locToCoords rLoc
      rLoc = robot ^. robotLocation
      worldCell =
        drawLoc
          (c ^. gameplay)
          g
          rCoords
      locStr = renderCoordsString rLoc

    statusWidget = case robot ^. machine of
      Waiting {} -> txt "waiting"
      _
        | isActive robot -> withAttr notifAttr $ txt "busy"
        | otherwise -> withAttr greenAttr $ txt "idle"

  basePos :: Point V2 Double
  basePos = realToFrac <$> fromMaybe origin (g ^? baseRobot . robotLocation . planar)
  -- Keep the base and non system robot (e.g. no seed)
  isRelevant robot = robot ^. robotID == 0 || not (robot ^. systemRobot)
  -- Keep the robot that are less than 32 unit away from the base
  isNear robot = creative || distance (realToFrac <$> robot ^. robotLocation . planar) basePos < 32
  robots :: [Robot]
  robots =
    filter (\robot -> debugAllRobots || (isRelevant robot && isNear robot))
      . IM.elems
      $ g ^. robotInfo . robotMap
  creative = g ^. creativeMode
  debugRID = c ^. uiDbg . Lens.contains ListRobotIDs
  debugAllRobots = c ^. uiDbg . Lens.contains ListAllRobots
  g = c ^. mygs
