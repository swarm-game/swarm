{-# LANGUAGE OverloadedStrings #-}

-- Display logic for Objectives.
module Swarm.Game.Scenario.Objective.Presentation.Render where

import Brick hiding (Direction, Location)
import Brick.Widgets.Center
import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
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
    if goalsCount > 1
      then
        hBox
          [ leftSide
          , hLimitPercent 70 goalElaboration
          ]
      else goalElaboration
 where
  lw = _listWidget gd
  leftSide =
    hLimitPercent 30 $
      vBox
        [ hCenter $ str "Goals"
        , padAll 1 $
            vLimit 10 $
              BL.renderList (const drawGoalListItem) True lw
        ]
  goalsCount = sum . M.elems . M.map NE.length . goals $ gd ^. goalsContent

  goalElaboration =
    padLeft (Pad 2) $
      maybe emptyWidget (singleGoalDetails . snd) $
        BL.listSelectedElement lw

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
    textSource = obj ^. objectiveTeaser <|> obj ^. objectiveId
    titleWidget = withEllipsis $ fromMaybe (NE.head $ obj ^. objectiveGoal) textSource

singleGoalDetails :: GoalEntry -> Widget Name
singleGoalDetails = \case
  Header _gs -> str " "
  Goal _gs obj -> displayParagraphs $ NE.toList $ obj ^. objectiveGoal
