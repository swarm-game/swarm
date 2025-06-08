{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for presentation of Robot details.
--
-- It stores robot IDs to identify rows, which makes
-- it relatively light-weight.
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
import Control.Lens as Lens hiding (Const, from)
import Data.IntMap qualified as IM
import Data.List.Extra (dropPrefix, enumerate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Linear (V2, distance)
import Numeric (showFFloat)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Entity as E
import Swarm.Game.Location (Point, origin)
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
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Robot.Details
import Swarm.TUI.View.Robot.Type
import Swarm.TUI.View.Shared (tabControlFooter)
import Swarm.Util (applyWhen, maximum0)
import Swarm.Util.UnitInterval
import Swarm.Util.WindowedCounter qualified as WC
import System.Clock (TimeSpec (..))

{--------------------------------------------------------------------
NEW GRID LIST
--------------------------------------------------------------------}

-- | The columns in the Robot modal grid.
data RobotColumn
  = ColName
  | ColAge
  | ColPos
  | ColItems
  | ColStatus
  | ColActns
  | ColCmds
  | ColCycles
  | ColActivity
  | ColLog
  | -- | The ID is the only optional field.
    --   It is shown last to make indexing code easier.
    ColID
  deriving (Eq, Ord, Enum, Bounded, Show)

colName :: RobotColumn -> Text
colName = T.pack . dropPrefix "Col" . show

colWidth :: RobotColumn -> Int
colWidth = textWidth . colName

colWidths :: Set DebugOption -> Seq ColWidth
colWidths opt = ColW . getWidth <$> S.fromList robotColumns
 where
  showIDs = Set.member ListRobotIDs opt
  robotColumns :: [RobotColumn]
  robotColumns = (if showIDs then id else filter (/= ColID)) enumerate
  getWidth = \case
    ColName -> 26 + (if showIDs then 0 else getWidth ColID)
    ColAge -> 8
    ColPos -> 9
    ColStatus -> 10
    ColID -> 5
    c -> 1 + colWidth c

{--------------------------------------------------------------------
EMPTY
--------------------------------------------------------------------}

-- | Initial empty robot modal.
emptyRobotDisplay :: Set DebugOption -> RobotDisplay
emptyRobotDisplay opt =
  RobotDisplay
    { _isDetailsOpened = False
    , -- we have to select the fixed width columns at start - if they change, the whole list has to be replaced
      _robotsGridList = BL.gridTabularList (RobotsListDialog RobotList) mempty (LstItmH 1) (colWidths opt)
    , _robotDetailsPaneState =
        RobotDetailsPaneState
          { _detailFocus = focusRing $ map (RobotsListDialog . SingleRobotDetails) enumerate
          , _logsList = BL.list (RobotsListDialog $ SingleRobotDetails RobotLogPane) mempty 1
          , _cmdHistogramList = BL.list (RobotsListDialog $ SingleRobotDetails RobotCommandHistogramPane) mempty 1
          }
    }

{--------------------------------------------------------------------
GET SELECTED
--------------------------------------------------------------------}

getSelectedRID :: BL.GridTabularList Name RID -> Maybe RID
getSelectedRID gl = snd <$> BL.listSelectedElement gl.list

-- | Get the robot selected in the robot list.
getSelectedRobot :: GameState -> BL.GridTabularList Name RID -> Maybe Robot
getSelectedRobot g gl = do
  rid <- getSelectedRID gl
  g ^. robotInfo . robotMap . at rid

{--------------------------------------------------------------------
UPDATE
--------------------------------------------------------------------}

-- | Update robot modal grid list contents.
--
-- To prevent memory leaks, this only stores robot IDs.
updateRobotList :: Set DebugOption -> GameState -> BL.GridTabularList Name RID -> BL.GridTabularList Name RID
updateRobotList dOpts g l = l {BL.list = updatedList}
 where
  updatedList :: BL.GenericList Name Seq RID
  updatedList = BL.listReplace rids sel l.list
  rids :: Seq RID
  rids = S.fromList . fmap (view robotID) $ robots
  sel :: Maybe Int
  sel = flip S.elemIndexL rids =<< getSelectedRID l

  robots :: [Robot]
  robots = g ^. robotInfo . robotMap . to IM.elems . to filterRobots
  filterRobots :: [Robot] -> [Robot]
  filterRobots = if Set.member ListAllRobots dOpts then id else filter (\r -> isRelevant r && isNear r)
  basePos :: Point V2 Double
  basePos = realToFrac <$> fromMaybe origin (g ^? baseRobot . robotLocation . planar)
  -- Keep the base and non system robot (e.g. no seed)
  isRelevant r = r ^. robotID == 0 || not (r ^. systemRobot)
  -- Keep the robot that are less than 32 unit away from the base
  isNear r = creative || distance (realToFrac <$> r ^. robotLocation . planar) basePos < 32
  creative = g ^. creativeMode

{--------------------------------------------------------------------
DRAW
--------------------------------------------------------------------}

columnHdrAttr :: AttrName
columnHdrAttr = attrName "columnHeader"

rowHdrAttr :: AttrName
rowHdrAttr = attrName "rowHeader"

-- | Draw robot modal.
--
-- It either shows a list of robots information or details of selected robot.
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

-- | Render robot modal grid - column and row headers as well as cells based on robot ID.
robotGridRenderers :: UIGameplay -> GameState -> BL.GridRenderers Name RID
robotGridRenderers t g =
  BL.GridRenderers
    { BL.cell = drawRobotGridCell t g
    , BL.rowHdr = Just rowHdr
    , BL.colHdr = Just colHdr
    , BL.colHdrRowHdr = Just colRowHdr
    }

-- | Enumerates the rows by position (just row index, not 'RID').
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
colRowHdr = BL.ColHdrRowHdr $ \_ (WdthD _wd) -> fill ' ' <=> hBorder

drawRobotGridCell :: UIGameplay -> GameState -> ListFocused -> WidthDeficit -> BL.GridCtxt -> RID -> Widget Name
drawRobotGridCell uig g _foc (WdthD widthDef) ctx rid =
  colGap . withSelectedAttr $
    case g ^. robotInfo . robotMap . at rid of
      -- this would be a synchronisation error, but crashing the game is not worth it
      Nothing -> case col of
        ColID -> padRight Max $ showW rid
        _ -> fill '?'
      Just r -> case col of
        ColName -> padRight Max $ nameWidget r
        ColAge -> hCenter $ ageWidget r
        ColPos -> hCenter $ locWidget r
        ColItems -> padLeft Max $ rInvCount r
        ColStatus -> hCenter $ statusWidget r
        ColActns -> padLeft Max . showW $ r ^. activityCounts . tangibleCommandCount
        ColCmds -> padLeft Max . showW . sum . M.elems $ r ^. activityCounts . commandsHistogram
        ColCycles -> padLeft Max . showW $ r ^. activityCounts . lifetimeStepCount
        ColActivity -> padLeft Max $ renderDutyCycle (g ^. temporal) r
        ColLog -> hCenter $ rLog r
        ColID -> padRight Max $ showW rid
 where
  (BL.GColC (BL.Ix cIx) (BL.Sel cSel)) = ctx.col
  (BL.GRowC (BL.Ix _ix) (BL.Sel rSel)) = ctx.row
  withSelectedAttr = if cSel && rSel then withAttr BL.listSelectedAttr else id
  col :: RobotColumn
  col = if cIx <= fromEnum (maxBound :: RobotColumn) then toEnum cIx else ColLog

  showW :: Show a => a -> Widget Name
  showW = str . show
  highlightSystem :: Robot -> Widget Name -> Widget Name
  highlightSystem r = applyWhen (r ^. systemRobot) $ withAttr highlightAttr
  colGap = padLeft (Pad $ if widthDef > 0 then 0 else 1)

  aMap = uig ^. uiAttributeMap

  nameWidget :: Robot -> Widget Name
  nameWidget r =
    hBox
      [ renderTexel (renderRobot aMap r)
      , highlightSystem r . txt $ " " <> r ^. robotName
      ]

  ageWidget :: Robot -> Widget Name
  ageWidget r = str ageStr
   where
    TimeSpec createdAtSec _ = r ^. robotCreatedAt
    TimeSpec nowSec _ = uig ^. uiTiming . lastFrameTime
    age = nowSec - createdAtSec
    ageStr
      | age < 60 = show age <> "sec"
      | age < 3600 = show (age `div` 60) <> "min"
      | age < 3600 * 24 = show (age `div` 3600) <> "hour"
      | otherwise = show (age `div` 3600 * 24) <> "day"

  rInvCount :: Robot -> Widget Name
  rInvCount r = showW . sum . map fst . E.elems $ r ^. robotEntity . entityInventory

  rLog :: Robot -> Widget Name
  rLog r = str $ if r ^. robotLogUpdated then "x" else " "

  locWidget :: Robot -> Widget Name
  locWidget r = hBox [worldCell, str $ " " <> locStr]
   where
    rCoords = fmap locToCoords rLoc
    rLoc = r ^. robotLocation
    worldCell = drawLoc uig g rCoords
    locStr = renderCoordsString rLoc

  statusWidget :: Robot -> Widget Name
  statusWidget r = case r ^. machine of
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
