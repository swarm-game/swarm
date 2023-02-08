{-# LANGUAGE OverloadedStrings #-}

-- Display logic for Objectives.
module Swarm.Game.Scenario.Objective.Presentation.Render where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Center
import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Vector qualified as V
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Presentation.Model
import Swarm.TUI.Attr
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Util

makeListWidget :: GoalTracking -> BL.List Name GoalEntry
makeListWidget (GoalTracking _announcements categorizedObjs) =
  BL.listMoveTo 1 $ BL.list (GoalWidgets ObjectivesList) (V.fromList objList) 1
 where
  objList = intercalate [Spacer] $ map f $ M.toList categorizedObjs
  f (h, xs) = Header h : map (Goal h) (NE.toList xs)

renderGoalsDisplay :: GoalDisplay -> Widget Name
renderGoalsDisplay gd =
  if hasMultiple
    then
      hBox
        [ leftSide
        , hLimitPercent 70 $ padLeft (Pad 2) goalElaboration
        ]
    else goalElaboration
 where
  hasMultiple = hasMultipleGoals $ gd ^. goalsContent
  lw = _listWidget gd
  fr = _focus gd
  leftSide =
    hLimitPercent 30 $
      padAll 1 $
        vBox
          [ hCenter $ str "Goals"
          , padAll 1 $
              vLimit 10 $
                withFocusRing fr (BL.renderList drawGoalListItem) lw
          ]

  -- Adds very subtle coloring to indicate focus switch
  highlightIfFocused = case (hasMultiple, focusGetCurrent fr) of
    (True, Just (GoalWidgets GoalSummary)) -> withAttr lightCyanAttr
    _ -> id

  -- Note: An extra "padRight" is inserted to account for the vertical scrollbar,
  -- whether or not it appears.
  goalElaboration =
    clickable (GoalWidgets GoalSummary) $
      maybeScroll ModalViewport $
        maybe emptyWidget (padAll 1 . padRight (Pad 1) . highlightIfFocused . singleGoalDetails . snd) $
          BL.listSelectedElement lw

getCompletionIcon :: Objective -> GoalStatus -> Widget Name
getCompletionIcon obj = \case
  Upcoming -> withAttr yellowAttr $ txt " ○  "
  Active -> withAttr cyanAttr $ txt " ○  "
  Failed -> withAttr redAttr $ txt " ●  "
  Completed -> withAttr colorAttr $ txt " ●  "
   where
    colorAttr =
      if obj ^. objectiveHidden
        then magentaAttr
        else greenAttr

drawGoalListItem ::
  Bool ->
  GoalEntry ->
  Widget Name
drawGoalListItem _isSelected e = case e of
  Spacer -> str " "
  Header gs -> withAttr boldAttr $ str $ show gs
  Goal gs obj -> getCompletionIcon obj gs <+> titleWidget
   where
    textSource = obj ^. objectiveTeaser <|> obj ^. objectiveId <|> listToMaybe (obj ^. objectiveGoal)
    titleWidget = maybe (txt "?") withEllipsis textSource

singleGoalDetails :: GoalEntry -> Widget Name
singleGoalDetails = \case
  Goal _gs obj -> displayParagraphs $ obj ^. objectiveGoal
  -- Only Goal entries are selectable, so we should never see this:
  _ -> emptyWidget
