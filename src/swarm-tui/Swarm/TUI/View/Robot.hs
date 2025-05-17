{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for presentation of Robot details.
module Swarm.TUI.View.Robot (
  emptyRobotDisplay,
  updateRobotList,
  robotGridRenderers,
  drawRobotsDisplayModal,
  getSelectedRobot,
) where

import Brick
import Brick.Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List qualified as BL
import Brick.Widgets.TabularList.Grid qualified as BL
import Brick.Widgets.TabularList.Mixed
import Brick.Widgets.TabularList.Types (ColWidth (..))
import Control.Lens (makeLenses, view, (^.), (^?))
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.List (mapAccumL)
import Data.List.Extra (dropPrefix, enumerate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Text (Text)
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
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Robot.Details
import Swarm.TUI.View.Robot.Type
import Swarm.TUI.View.Shared (tabControlFooter)
import Swarm.Util (applyWhen, maximum0, maximumNE)
import Swarm.Util.UnitInterval
import Swarm.Util.WindowedCounter qualified as WC
import System.Clock (TimeSpec (..))

{--------------------------------------------------------------------
NEW GRID LIST
--------------------------------------------------------------------}

data RobotColumn
  = ColID
  | ColName
  | ColAge
  | ColPos
  | ColItems
  | ColStatus
  | ColActns
  | ColCmds
  | ColCycles
  | ColActivity
  | ColLog
  deriving (Eq, Ord, Enum, Bounded, Show)

colName :: RobotColumn -> Text
colName = T.pack . dropPrefix "Col" . show

colWidth :: RobotColumn -> Int
colWidth = textWidth . colName

-- { rowID = Col "ID" Minimal
-- , rowName = Col "Name" Grow
-- , rowAge = Col "Age" Minimal
-- , rowPos = Col "Pos" Minimal
-- , rowItems = Col "Items" Minimal
-- , rowStatus = Col "Status" Minimal
-- , rowActns = Col "Actns" Minimal
-- , rowCmds = Col "Cmds" Minimal
-- , rowCycles = Col "Cycles" Minimal
-- , rowActivity = Col "Activity" Grow
-- , rowLog = Col "Log" Minimal
colWidths :: Seq ColWidth
colWidths =
  S.fromList enumerate
    <&> ColW . \case
      ColID -> 5
      ColName -> 25
      ColAge -> 7
      ColPos -> 9
      ColStatus -> 10
      c -> 1 + colWidth c

{--------------------------------------------------------------------
EMPTY
--------------------------------------------------------------------}

emptyRobotDisplay :: RobotDisplay
emptyRobotDisplay =
  RobotDisplay
    { _isDetailsOpened = False
    , _robotsGridList = BL.gridTabularList (RobotsListDialog RobotList) mempty (LstItmH 1) colWidths
    , _robotDetailsPaneState =
        RobotDetailsPaneState
          { _detailFocus = focusRing $ map (RobotsListDialog . SingleRobotDetails) enumerate
          , _logsList = BL.list (RobotsListDialog $ SingleRobotDetails RobotLogPane) mempty 1
          , _cmdHistogramList = BL.list (RobotsListDialog $ SingleRobotDetails RobotCommandHistogramPane) mempty 1
          }
    }

{--------------------------------------------------------------------
UPDATE
--------------------------------------------------------------------}

getSelectedRID :: BL.GridTabularList Name RID -> Maybe RID
getSelectedRID gl = snd <$> BL.listSelectedElement gl.list

getSelectedRobot :: GameState -> BL.GridTabularList Name RID -> Maybe Robot
getSelectedRobot g gl = do
  rid <- getSelectedRID gl
  g ^. robotInfo . robotMap . at rid

updateRobotList :: GameState -> BL.GridTabularList Name RID -> BL.GridTabularList Name RID
updateRobotList g l = l {BL.list = updatedList}
 where
  updatedList :: BL.GenericList Name Seq RID
  updatedList = BL.listReplace rids sel l.list
  rids :: Seq RID
  rids = g ^. robotInfo . robotMap . to IM.keys . to S.fromList
  sel :: Maybe Int
  sel = flip S.elemIndexL rids =<< getSelectedRID l

{--------------------------------------------------------------------
DRAW
--------------------------------------------------------------------}

columnHdrAttr :: AttrName
columnHdrAttr = attrName "columnHeader"

rowHdrAttr :: AttrName
rowHdrAttr = attrName "rowHeader"

drawRobotsDisplayModal :: UIGameplay -> GameState -> RobotDisplay -> Widget Name
drawRobotsDisplayModal t g robDisplay =
  if robDisplay ^. isDetailsOpened
    then robDetail
    else robList
 where
  robDetail :: Widget Name
  robDetail =
    let detailsContent = case getSelectedRobot g (robDisplay ^. robotsGridList) of
          Nothing -> str "No selection"
          Just r -> renderRobotDetails r $ robDisplay ^. robotDetailsPaneState
     in vBox
          [ detailsContent
          , tabControlFooter
          ]
  robList :: Widget Name
  robList = drawRobotsList t g $ robDisplay ^. robotsGridList

drawRobotsList :: UIGameplay -> GameState -> BL.GridTabularList Name RID -> Widget Name
drawRobotsList t g = vLimit 30 . BL.renderGridTabularList (robotGridRenderers t g) (LstFcs True)

robotGridRenderers :: UIGameplay -> GameState -> BL.GridRenderers Name RID
robotGridRenderers t g =
  BL.GridRenderers
    { BL.cell = drawRobotGridCell t g
    , BL.rowHdr = Just rowHdr
    , BL.colHdr = Just colHdr
    , BL.colHdrRowHdr = Just colRowHdr
    }

-- | Enumerates the rows by position (not 'RID').
rowHdr :: RowHdr Name a
rowHdr =
  RowHdr
    { draw = \_ (WdthD wd) (RowHdrCtxt (Sel s)) rh ->
        let attrFn = applyWhen (not s) $ withAttr rowHdrAttr
         in attrFn $ padRight (Pad $ if wd > 0 then 0 else 1) $ padLeft Max (str $ show rh)
    , width = \_ rh -> RowHdrW . (+ 2) . maximum0 $ map (length . show) rh
    , toRH = \_ (Ix i) -> i + 1
    }

colHdr :: BL.GridColHdr Name
colHdr =
  BL.GridColHdr
    { draw =
        \_ (WdthD widthDef) (BL.GColC (BL.Ix i) (BL.Sel _sel)) ->
          let colGap = padLeft (Pad $ if widthDef > 0 then 0 else 1)
           in withAttr columnHdrAttr (colGap . txt . colName $ toEnum i) <=> hBorder
    , height = ColHdrH 2
    }

colRowHdr :: BL.ColHdrRowHdr Name
colRowHdr = BL.ColHdrRowHdr $ \_ (WdthD _wd) -> (fill ' ') <=> hBorder

drawRobotGridCell :: UIGameplay -> GameState -> ListFocused -> WidthDeficit -> BL.GridCtxt -> RID -> Widget Name
drawRobotGridCell t g _foc (WdthD widthDef) ctx rid =
  colGap . withSelectedAttr $ case col of
    ColID -> padRight Max $ showW $ r ^. robotID
    ColName -> padRight Max $ nameWidget
    ColAge -> hCenter $ ageWidget
    ColPos -> hCenter $ locWidget
    ColItems -> padLeft Max $ rInvCount -- increaseWidth 1 $
    ColStatus -> hCenter $ statusWidget
    ColActns -> padLeft Max . showW $ r ^. activityCounts . tangibleCommandCount
    ColCmds -> padLeft Max . showW . sum . M.elems $ r ^. activityCounts . commandsHistogram
    ColCycles -> padLeft Max . showW $ r ^. activityCounts . lifetimeStepCount
    ColActivity -> padLeft Max $ renderDutyCycle (g ^. temporal) r
    ColLog -> hCenter $ rLog
 where
  (BL.GColC (BL.Ix cIx) (BL.Sel cSel)) = ctx.col
  (BL.GRowC (BL.Ix _ix) (BL.Sel rSel)) = ctx.row
  withSelectedAttr = if cSel && rSel then withAttr BL.listSelectedAttr else id
  col :: RobotColumn
  col = toEnum cIx

  r :: Robot
  r = case g ^. robotInfo . robotMap . at rid of
    Just jr -> jr
    Nothing -> error $ "Could not find RID in map: " <> show rid

  showW :: Show a => a -> Widget Name
  showW = str . show
  highlightSystem :: Widget Name -> Widget Name
  highlightSystem = applyWhen (r ^. systemRobot) $ withAttr highlightAttr
  colGap = padLeft (Pad $ if widthDef > 0 then 0 else 1)

  -- WithWidth (2 + T.length nameTxt)
  nameWidget :: Widget Name
  nameWidget =
    hBox
      [ renderDisplay (r ^. robotDisplay)
      , highlightSystem . txt $ " " <> r ^. robotName
      ]

  ageWidget :: Widget Name
  ageWidget = str ageStr
   where
    TimeSpec createdAtSec _ = r ^. robotCreatedAt
    TimeSpec nowSec _ = t ^. uiTiming . lastFrameTime
    age = nowSec - createdAtSec
    ageStr
      | age < 60 = show age <> "sec"
      | age < 3600 = show (age `div` 60) <> "min"
      | age < 3600 * 24 = show (age `div` 3600) <> "hour"
      | otherwise = show (age `div` 3600 * 24) <> "day"

  rInvCount :: Widget Name
  rInvCount = showW . sum . map fst . E.elems $ r ^. robotEntity . entityInventory

  rLog :: Widget Name
  rLog = str $ if r ^. robotLogUpdated then "x" else " "

  -- WithWidth (2 + length locStr)
  locWidget :: Widget Name
  locWidget = hBox [worldCell, str $ " " <> locStr]
   where
    rCoords = fmap locToCoords rLoc
    rLoc = r ^. robotLocation
    worldCell = drawLoc t g rCoords
    locStr = renderCoordsString rLoc

  statusWidget :: Widget Name
  statusWidget = case r ^. machine of
    Waiting {} -> str "waiting"
    _ | isActive r -> withAttr notifAttr $ str "busy"
    _ | otherwise -> withAttr greenAttr $ str "idle"

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
renderDutyCycle temporalState r = withAttr dutyCycleAttr $ str tx
 where
  tx = showFFloat (Just 1) dutyCyclePercentage "%"

  curTicks = temporalState ^. ticks
  window = r ^. activityCounts . activityWindow

  -- Rewind to previous tick
  latestRobotTick = addTicks (-1) curTicks
  dutyCycleRatio = WC.getOccupancy latestRobotTick window

  dutyCycleAttr = safeIndex dutyCycleRatio meterAttributeNames

  dutyCyclePercentage :: Double
  dutyCyclePercentage = 100 * getValue dutyCycleRatio
