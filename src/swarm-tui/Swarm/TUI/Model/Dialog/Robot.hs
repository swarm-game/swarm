{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Dialog.Robot where

import Data.Maybe (fromMaybe)
import Swarm.Game.Robot.Activity
import Swarm.Game.Robot.Concrete
import Swarm.Game.State.Robot
import Swarm.Game.World.Coords
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Robot

-- import Swarm.TUI.View.Util

import Brick
import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Widgets.Table qualified as BT
import Brick.Widgets.TabularList.Mixed
import Control.Lens hiding (from, (<.>))
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Linear (V2 (..), distance)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Entity as E
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.TUI.Model.DebugOption
import Swarm.TUI.Model.Dialog.RobotDisplay
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.Util (applyWhen)
import System.Clock (TimeSpec (..))

data RobotRenderingContext = RobotRenderingContext
  { _mygs :: GameState
  , _gameplay :: UIGameplay
  , _timing :: UITiming
  , _uiDbg :: Set DebugOption
  }

makeLenses ''RobotRenderingContext

emptyRobotDisplay :: RobotDisplay
emptyRobotDisplay =
  RobotDisplay
    { _robotsDisplayMode = RobotList
    , _lastFocusedRobotId = Nothing
    , _libList = mixedTabularList RobotsList libraryEntries (LstItmH 1) wprk wpr
    , _libRenderers =
        MixedRenderers
          { cell = dc
          , rowHdr = Just rowHdr
          , colHdr = Just colHdr
          , colHdrRowHdr = Just $ ColHdrRowHdr $ \_ _ -> vLimit 1 (fill ' ') <=> hBorder
          }
    }

renderTheRobots :: RobotDisplay -> Widget Name
renderTheRobots rd = renderMixedTabularList (rd ^. libRenderers) (LstFcs True) (rd ^. libList)

columnHdrAttr :: AttrName
columnHdrAttr = attrName "columnHeader"

rowHdrAttr :: AttrName
rowHdrAttr = attrName "rowHeader"

colHdr :: MixedColHdr Name Widths
colHdr =
  MixedColHdr
    { draw = \_ (MColC (Ix ci)) -> case colHdrs V.!? ci of
        Just ch -> withAttr columnHdrAttr (padRight Max (str ch) <+> str " ") <=> hBorder
        Nothing -> emptyWidget
    , widths = \(Widths song) -> song
    , height = ColHdrH 2
    }

rowHdr :: RowHdr Name LibRobotRow
rowHdr =
  RowHdr
    { draw = \_ (WdthD wd) (RowHdrCtxt (Sel s)) rh ->
        let attrFn =
              if s
                then id
                else withAttr rowHdrAttr
         in attrFn $ padRight (Pad $ if wd > 0 then 0 else 1) $ padLeft Max (str $ show rh)
    , width = \_ rh -> RowHdrW $ (+ 2) $ maximum $ map (length . show) rh
    , toRH = \_ (Ix i) -> i + 1
    }

colHdrs :: Vector String
colHdrs = V.fromList ["Artist", "Title", "Album"]

wpr :: WidthsPerRow LibRobotRow Widths
wpr = WsPerR $ \(Widths song) e -> case e of
  LibRobotRow {} -> song

dc :: ListFocused -> MixedCtxt -> LibRobotRow -> Widget Name
dc _ (MxdCtxt _ (MColC (Ix ci))) r =
  let renderPlainCell s = padRight Max (str s) <+> str " "
   in case ci of
        0 -> _fName r
        1 -> _fAge r
        2 -> _fPos r
        3 -> _fItems r
        4 -> _fStatus r
        5 -> _fActns r
        6 -> _fCmds r
        7 -> _fCycles r
        8 -> _fActivity r
        9 -> _fLog r
        _ -> emptyWidget

libraryEntries :: Seq LibRobotRow
libraryEntries =
  let songs =
        map
          (\n -> LibRobotRow (str ("foo" ++ show n)) (str "bar") (str "blah") (str "blah") (str "blah") (str "blah") (str "blah") (str "blah") (str "blah") (str "blah"))
          [1 .. 12 :: Int]
   in S.fromList songs

wprk :: WidthsPerRowKind LibRobotRow Widths
wprk = WsPerRK $ \(AvlW aW) _ ->
  let artist = max 7 $ (aW * 30) `div` 100
      title = max 6 $ (aW * 30) `div` 100
      album = aW - artist - title
   in Widths {robotRowWidths = fmap ColW [artist, title, album]}

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
