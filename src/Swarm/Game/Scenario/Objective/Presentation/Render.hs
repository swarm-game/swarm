{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Scenario.Objective.Presentation.Render where

import Swarm.Game.Scenario.Objective
import Swarm.TUI.View.Util
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List (intersperse, intercalate)
import Control.Lens hiding (Const, from)
import Brick hiding (Direction, Location)
import Data.Map.Strict qualified as M
import Swarm.TUI.Model.Name
import Swarm.Game.Scenario.Objective.Presentation.Model

renderGoalsDisplay :: GoalDisplay -> Widget Name
renderGoalsDisplay (GoalDisplay _announcements categorizedObjs) = 
  vBox $ intercalate [txt "========="] $
    map renderGoalsCategory $ M.toList categorizedObjs
  where
    renderGoalsCategory :: (GoalStatus, NonEmpty Objective) -> [Widget Name]
    renderGoalsCategory (category, objs) =
        header : intersperse (txt "--------") (objectiveTexts objs)
      where
        header = str $ unwords [">>", show category, "<<"]

    objectiveTexts :: NonEmpty Objective -> [Widget Name]
    objectiveTexts = map renderSingleGoal . NE.toList

renderSingleGoal :: Objective -> Widget Name
renderSingleGoal g = displayParagraphs $ g ^. objectiveGoal