{-# LANGUAGE OverloadedStrings #-}

-- Display logic for Objectives.
module Swarm.Game.Scenario.Objective.Presentation.Render where

import Brick hiding (Direction, Location)
import Brick.Widgets.Center
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Presentation.Model
import Swarm.TUI.Attr
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Util

makeListWidget :: GoalTracking -> BL.List Name GoalEntry
makeListWidget (GoalTracking _announcements categorizedObjs) =
  BL.listMoveTo 1 $ BL.list ObjectivesList (V.fromList objList) 1
 where
  objList = concatMap f $ M.toList categorizedObjs
  f (h, xs) = Header h : map (Goal h) (NE.toList xs)

renderGoalsDisplay :: GoalDisplay -> Widget Name
renderGoalsDisplay gd =
  padAll 1 $
    hBox
      [ hLimitPercent 30 $
          vBox
            [ hCenter $ str "Goals"
            , padAll 1 $
                vLimit 10 $
                  BL.renderList (const drawGoalListItem) True lw
            ]
      , hLimitPercent 70 $
          padLeft (Pad 2) $
            maybe emptyWidget (singleGoalDetails . snd) $
              BL.listSelectedElement lw
      ]
 where
  lw = _listWidget gd

withEllipsis :: Text -> Widget Name
withEllipsis t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        ellipsis = T.replicate 3 $ T.singleton '.'
        tLength = T.length t
        newText =
          if tLength > w
            then T.take (w - T.length ellipsis) t <> ellipsis
            else t
    render $ txt newText

getCompletionIcon :: Objective -> GoalStatus -> Widget Name
getCompletionIcon obj = \case
  Upcoming -> withAttr yellowAttr $ txt " ○  "
  Active -> withAttr cyanAttr $ txt " ○  "
  Failed -> withAttr redAttr $ txt " ●  "
  Completed -> withAttr colorattr $ txt " ●  "
   where
    colorattr =
      if obj ^. objectiveHidden
        then magentaAttr
        else greenAttr

drawGoalListItem ::
  GoalEntry ->
  Widget Name
drawGoalListItem = \case
  Header gs -> withAttr boldAttr $ str $ show gs
  Goal gs obj -> getCompletionIcon obj gs <+> titleWidget
   where
    titleWidget = case listToMaybe $ obj ^. objectiveGoal of
      Nothing -> txt "?"
      Just x -> withEllipsis x

singleGoalDetails :: GoalEntry -> Widget Name
singleGoalDetails = \case
  Header _gs -> displayParagraphs [" "]
  Goal _gs obj -> displayParagraphs $ obj ^. objectiveGoal
