{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for Structure presentation.
module Swarm.TUI.Model.Dialog.Robot where

import Brick
import Control.Lens hiding (from, (<.>))
import Swarm.Game.Robot

import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Widgets.TabularList.Mixed
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Swarm.TUI.Model.Name

data RobotsDisplayMode = RobotList | SingleRobotDetails
  deriving (Eq, Show, Enum, Bounded)

newtype Widths = Widths
  { robotRowWidths :: [ColWidth]
  }
  deriving (Generic)

data LibRobotRow = LibRobotRow String String String

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

dc :: ListFocused -> MixedCtxt -> LibRobotRow -> Widget n
dc _ (MxdCtxt _ (MColC (Ix ci))) (LibRobotRow c1 c2 c3) =
  let renderPlainCell s = padRight Max (str s) <+> str " "
   in case ci of
        0 -> renderPlainCell c1
        1 -> renderPlainCell c2
        2 -> renderPlainCell c3
        _ -> emptyWidget

libraryEntries :: Seq LibRobotRow
libraryEntries =
  let songs =
        map
          (\n -> LibRobotRow ("foo" ++ show n) "bar" "blah")
          [1 .. 12 :: Int]
   in S.fromList songs

wprk :: WidthsPerRowKind LibRobotRow Widths
wprk = WsPerRK $ \(AvlW aW) _ ->
  let artist = max 7 $ (aW * 30) `div` 100
      title = max 6 $ (aW * 30) `div` 100
      album = aW - artist - title
   in Widths {robotRowWidths = fmap ColW [artist, title, album]}
