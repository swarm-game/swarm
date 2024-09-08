{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Dialog.Robot where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.TabularList.Mixed
import Control.Lens hiding (from, (<.>))
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.List (transpose)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Linear (V2 (..), distance)
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
import Swarm.TUI.Model.DebugOption
import Swarm.TUI.Model.Dialog.RobotDisplay
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.Util (applyWhen, maximum0)
import Swarm.Util.UnitInterval
import Swarm.Util.WindowedCounter qualified as WC
import System.Clock (TimeSpec (..))

instance Semigroup ColWidth where
  ColW w1 <> ColW w2 = ColW $ max w1 w2

data RobotRenderingContext = RobotRenderingContext
  { _mygs :: GameState
  , _gameplay :: UIGameplay
  , _timing :: UITiming
  , _uiDbg :: Set DebugOption
  }

makeLenses ''RobotRenderingContext

mkRobotDisplay :: RobotRenderingContext -> RobotDisplay
mkRobotDisplay c =
  RobotDisplay
    { _robotsDisplayMode = RobotList
    , _lastFocusedRobotId = Nothing
    , _libList = mixedTabularList RobotsList (mkLibraryEntries c) (LstItmH 1) wprk wpr
    , _libRenderers =
        MixedRenderers
          { cell = dc
          , rowHdr = Just rowHdr
          , colHdr = Just $ colHdr $ c ^. uiDbg
          , colHdrRowHdr = Just $ ColHdrRowHdr $ \_ _ -> vLimit 1 (fill ' ') <=> hBorder
          }
    }

emptyRobotDisplay :: Set DebugOption -> RobotDisplay
emptyRobotDisplay uiDebug =
  RobotDisplay
    { _robotsDisplayMode = RobotList
    , _lastFocusedRobotId = Nothing
    , _libList = mixedTabularList RobotsList mempty (LstItmH 1) wprk wpr
    , _libRenderers =
        MixedRenderers
          { cell = dc
          , rowHdr = Just rowHdr
          , colHdr = Just $ colHdr uiDebug
          , colHdrRowHdr = Just $ ColHdrRowHdr $ \_ _ -> vLimit 1 (fill ' ') <=> hBorder
          }
    }

renderTheRobots :: RobotDisplay -> Widget Name
renderTheRobots rd =
  renderMixedTabularList (rd ^. libRenderers) (LstFcs True) (rd ^. libList)

columnHdrAttr :: AttrName
columnHdrAttr = attrName "columnHeader"

rowHdrAttr :: AttrName
rowHdrAttr = attrName "rowHeader"

colHdr :: Set DebugOption -> MixedColHdr Name Widths
colHdr uiDbg =
  MixedColHdr
    { draw = \_ (MColC (Ix ci)) -> case hdrs V.!? ci of
        -- Just ch -> withAttr columnHdrAttr (padRight Max (str ch) <+> str " ") <=> hBorder
        Just ch -> withAttr columnHdrAttr (padRight Max $ str ch) <=> hBorder
        Nothing -> emptyWidget
    , widths = \(Widths ws) -> zipWith (<>) ws (map (ColW . length) $ V.toList hdrs)
    , height = ColHdrH 2
    }
 where
  hdrs = colHdrs uiDbg

-- | Enumerates the rows by position (not 'RID').
rowHdr :: RowHdr Name RobotWidgetRow
rowHdr =
  RowHdr
    { draw = \_ (WdthD wd) (RowHdrCtxt (Sel s)) rh ->
        let attrFn =
              if s
                then id
                else withAttr rowHdrAttr
         in attrFn $ padRight (Pad $ if wd > 0 then 0 else 1) $ padLeft Max (str $ show rh)
    , width = \_ rh -> RowHdrW . (+ 2) . maximum0 $ map (length . show) rh
    , toRH = \_ (Ix i) -> i + 1
    }

headingStringList :: [String]
headingStringList =
  map ($ headingStrings) accessorList
 where
  headingStrings =
    LibRobotRow
      { _fID = "ID"
      , _fName = "Name"
      , _fAge = "Age"
      , _fPos = "Pos"
      , _fItems = "Items"
      , _fStatus = "Status"
      , _fActns = "Actns"
      , _fCmds = "Cmds"
      , _fCycles = "Cycles"
      , _fActivity = "Activity"
      , _fLog = "Log"
      }

dropFirstColumn :: Set DebugOption -> [a] -> [a]
dropFirstColumn uiDebug =
  applyWhen (not debugRID) (drop 1)
 where
  debugRID = uiDebug ^. Lens.contains ListRobotIDs

colHdrs :: Set DebugOption -> Vector String
colHdrs uiDebug = V.fromList $ dropFirstColumn uiDebug headingStringList

accessorList :: [LibRobotRow a -> a]
accessorList =
  [ _fID
  , _fName
  , _fAge
  , _fPos
  , _fItems
  , _fStatus
  , _fActns
  , _fCmds
  , _fCycles
  , _fActivity
  , _fLog
  ]

indexedAccessors :: Vector (LibRobotRow a -> a)
indexedAccessors = V.fromList accessorList

dc :: ListFocused -> MixedCtxt -> RobotWidgetRow -> Widget Name
dc _ (MxdCtxt _ (MColC (Ix ci))) r =
  maybe emptyWidget (renderPlainCell . wWidget . ($ r)) (indexedAccessors V.!? ci)
 where
  renderPlainCell = padRight Max

-- | For a single-constructor datatype like 'RobotWidgetRow',
-- this implementation is trivial.
wpr :: WidthsPerRow RobotWidgetRow Widths
wpr = WsPerR $ \(Widths x) _ -> x

wprk :: WidthsPerRowKind RobotWidgetRow Widths
wprk = WsPerRK $ \(AvlW aW) allRows ->
  Widths {robotRowWidths = mkWidths allRows}
 where
  colHeaderRowLengths = map length headingStringList
  mkWidths = map (ColW . (+ 1) . maximum0) . transpose . (colHeaderRowLengths :) . map getColWidthsForRow
   where
    getColWidthsForRow :: RobotWidgetRow -> [Int]
    getColWidthsForRow r = map (wWidth . ($ r)) accessorList

mkLibraryEntries :: RobotRenderingContext -> Seq RobotWidgetRow
mkLibraryEntries c =
  mkRobotRow <$> S.fromList robots
 where
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
  debugAllRobots = c ^. uiDbg . Lens.contains ListAllRobots
  g = c ^. mygs

  mkRobotRow robot =
    LibRobotRow
      { _fID =
          let tx = show $ robot ^. robotID
           in WidthWidget (length tx) (str tx)
      , _fName = nameWidget
      , _fAge = WidthWidget (length ageStr) (str ageStr)
      , _fPos = locWidget
      , _fItems =
          let tx = show rInvCount
           in WidthWidget (length tx) (padRight (Pad 1) (str tx))
      , _fStatus = statusWidget
      , _fActns =
          let tx = show $ robot ^. activityCounts . tangibleCommandCount
           in WidthWidget (length tx) (str tx)
      , -- TODO(#1341): May want to expose the details of this histogram in
        -- a per-robot pop-up
        _fCmds = strWidget $ show . sum . M.elems $ robot ^. activityCounts . commandsHistogram
      , _fCycles = strWidget $ show $ robot ^. activityCounts . lifetimeStepCount
      , _fActivity = renderDutyCycle (c ^. mygs . temporal) robot
      , _fLog = WidthWidget (T.length rLog) (txt rLog)
      }
   where
    strWidget tx = WidthWidget (length tx) (str tx)

    nameWidget =
      WidthWidget (2 + T.length nameTxt) w
     where
      w =
        hBox
          [ renderDisplay (robot ^. robotDisplay)
          , highlightSystem . txt $ " " <> nameTxt
          ]
      nameTxt = robot ^. robotName

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

    locWidget =
      WidthWidget (2 + length locStr) w
     where
      w = hBox [worldCell, str $ " " <> locStr]
      rCoords = fmap locToCoords rLoc
      rLoc = robot ^. robotLocation
      worldCell =
        drawLoc
          (c ^. gameplay)
          g
          rCoords
      locStr = renderCoordsString rLoc

    statusWidget = case robot ^. machine of
      Waiting {} ->
        let tx = "waiting"
         in WidthWidget (length tx) (str tx)
      _
        | isActive robot ->
            let tx = "busy"
             in WidthWidget (length tx) (withAttr notifAttr $ str tx)
        | otherwise ->
            let tx = "idle"
             in WidthWidget (length tx) (withAttr greenAttr $ str tx)

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
renderDutyCycle :: TemporalState -> Robot -> WidthWidget
renderDutyCycle temporalState robot =
  WidthWidget (length tx) (withAttr dutyCycleAttr $ str tx)
 where
  tx = showFFloat (Just 1) dutyCyclePercentage "%"

  curTicks = temporalState ^. ticks
  window = robot ^. activityCounts . activityWindow

  -- Rewind to previous tick
  latestRobotTick = addTicks (-1) curTicks
  dutyCycleRatio = WC.getOccupancy latestRobotTick window

  dutyCycleAttr = safeIndex dutyCycleRatio meterAttributeNames

  dutyCyclePercentage :: Double
  dutyCyclePercentage = 100 * getValue dutyCycleRatio
