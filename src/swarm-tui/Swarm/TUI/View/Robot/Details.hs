-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Robot.Details (renderRobotDetails) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.Table qualified as BT

import Brick.Focus
import Control.Lens hiding (from, (<.>))
import Data.Map qualified as M
import Prettyprinter (pretty)
import Swarm.Game.Robot
import Swarm.Game.Robot.Activity (commandsHistogram)
import Swarm.Game.Robot.Concrete
import Swarm.Language.Pretty (prettyText)
import Swarm.Log
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Attribute.Attr (boldAttr, cyanAttr)
import Swarm.TUI.View.Robot.Type

renderRobotDetails :: FocusRing Name -> Robot -> RobotDetailsPaneState -> Widget Name
renderRobotDetails ring robot paneState =
  vBox
    [ str $
        unwords
          [ "Selected robot"
          , show $ view robotName robot
          ]
    , hBorder
    , str " "
    , hBox $
        map
          hCenter
          [ hLimitPercent 70 $ highlightBorderFor RobotLogPane $ borderWithLabel (str "Logs") logsTable
          , hLimitPercent 30 $ highlightBorderFor RobotCommandHistogramPane $ borderWithLabel (str "Commands") commandsTable
          ]
    ]
 where
  highlightBorderFor n =
    if isFocused then overrideAttr borderAttr cyanAttr else id
   where
    isFocused = focusGetCurrent ring == Just (RobotsListDialog $ SingleRobotDetails n)

  logsTable = withFocusRing ring (BL.renderList mkLogTableEntry) $ paneState ^. logsList

  mkLogTableEntry _isSelected x =
    hBox
      [ withAttr cyanAttr . str . show . pretty . view leTime $ x
      , str ": "
      , txt . view leText $ x
      ]

  commandsTable =
    BT.renderTable
      . BT.columnBorders True
      . BT.rowBorders False
      . BT.surroundingBorder False
      . BT.setDefaultColAlignment BT.AlignLeft
      . BT.setColAlignment BT.AlignRight 0
      . BT.table
      $ map (withAttr boldAttr . str) ["Command", "Count"] : commandHistogramEntries

  mkHistogramEntry (k, v) =
    [ txt $ prettyText k
    , str $ show v
    ]

  commandHistogramEntries =
    map mkHistogramEntry $
      M.toList $
        robot ^. activityCounts . commandsHistogram
