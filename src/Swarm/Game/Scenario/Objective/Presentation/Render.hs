{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Scenario.Objective.Presentation.Render where

import Data.Vector qualified as V
import Brick hiding (Direction, Location)
import Control.Lens hiding (Const, from)
import Data.Maybe (listToMaybe)
import Brick.Widgets.List qualified as BL
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Presentation.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Util
import Swarm.TUI.Attr (greenAttr, boldAttr)

data GoalEntry
  = Header GoalStatus
  | Goal GoalStatus Objective

makeListWidget :: GoalDisplay -> BL.List Name GoalEntry
makeListWidget (GoalDisplay _announcements categorizedObjs) =
  BL.listMoveTo 1 $ BL.list ObjectivesList (V.fromList objList) 1
  where
    objList = concatMap f $ M.toList categorizedObjs
    f (h, xs) = Header h : map (Goal h) (NE.toList xs)


renderGoalsDisplay :: GoalDisplay -> Widget Name
renderGoalsDisplay gd =
  hBox
    [ padAll 2 $
          BL.renderList (const drawAchievementListItem) True listWidget
    , maybe emptyWidget (singleGoalDetails . snd) $
          BL.listSelectedElement listWidget
    ]
  where
    listWidget = makeListWidget gd

getCompletionIcon :: Bool -> Widget Name
getCompletionIcon = \case
  False -> txt " ○  "
  True -> withAttr greenAttr $ txt " ●  "

drawAchievementListItem ::
  GoalEntry ->
  Widget Name
drawAchievementListItem = \case
  Header gs -> withAttr boldAttr $ str $ show gs
  Goal _gs obj -> getCompletionIcon True <+> titleWidget
    where
      titleWidget = case listToMaybe $ obj ^. objectiveGoal of
        Nothing -> txt "?"
        Just x -> txt x


singleGoalDetails :: GoalEntry -> Widget Name
singleGoalDetails = \case
  Header gs -> emptyWidget
  Goal gs obj -> displayParagraphs $ obj ^. objectiveGoal
