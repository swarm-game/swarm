module Swarm.Game.Scenario.Objective.Display where

import Data.Text (Text)
import Swarm.Game.Scenario.Objective

-- | TODO Could also add an "ObjectiveFailed" constructor...
newtype Announcement
  = ObjectiveCompleted Objective

data GoalDisplay = GoalDisplay {
    announcements :: [Announcement]
  , prose :: [Text]
  }