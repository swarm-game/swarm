{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Scenario.Objective.Presentation.Model where

import Brick.Widgets.List qualified as BL
import Control.Lens (makeLenses)
import Data.Aeson
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.WinCheck
import Swarm.TUI.Model.Name

-- | These are intended to be used as keys in a map
-- of lists of goals.
data GoalStatus
  = -- | Goals in this category have other goals as prerequisites.
    -- However, they are only displayed if the "previewable" attribute
    -- is `true`.
    Upcoming
  | -- | Goals in this category may be pursued in parallel.
    -- However, they are only displayed if the "hidden" attribute
    -- is `false`.
    Active
  | -- | A goal's programmatic condition, as well as all its the prerequisites, were completed.
    -- This is a "latch" mechanism; at some point the conditions required to meet the goal may
    -- no longer hold. Nonetheless, the goal remains "completed".
    Completed
  | -- | A goal that can no longer be achieved.
    -- If this goal is not an "optional" goal, then the player
    -- also "Loses" the scenario.
    --
    -- Note that currently the only way to "Fail" a goal is by way
    -- of a negative prerequisite that was completed.
    Failed
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON, ToJSONKey)

-- | TODO Could also add an "ObjectiveFailed" constructor...
newtype Announcement
  = ObjectiveCompleted Objective
  deriving (Show, Generic, ToJSON)

type CategorizedGoals = Map GoalStatus (NonEmpty Objective)

data GoalEntry
  = Header GoalStatus
  | Goal GoalStatus Objective

data GoalTracking = GoalTracking
  { announcements :: [Announcement]
  -- ^ TODO the actual contents of these are not used yet,
  -- other than as a flag to pop up the Goal dialog.
  , goals :: CategorizedGoals
  }
  deriving (Generic, ToJSON)

data GoalDisplay = GoalDisplay
  { _goalsContent :: GoalTracking
  , _listWidget :: BL.List Name GoalEntry
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  }

makeLenses ''GoalDisplay

emptyGoalDisplay :: GoalDisplay
emptyGoalDisplay =
  GoalDisplay (GoalTracking mempty mempty) $
    BL.list ObjectivesList mempty 1

hasAnythingToShow :: GoalTracking -> Bool
hasAnythingToShow (GoalTracking ann g) = not (null ann && null g)

constructGoalMap :: Bool -> ObjectiveCompletion -> CategorizedGoals
constructGoalMap isCheating objectiveCompletion@(ObjectiveCompletion buckets _) =
  M.fromList $
    mapMaybe (traverse nonEmpty) categoryList
 where
  categoryList =
    [ (Upcoming, displayableInactives)
    , (Active, suppressHidden activeGoals)
    , (Completed, completed buckets)
    , (Failed, unwinnable buckets)
    ]

  displayableInactives =
    suppressHidden $
      filter (maybe False previewable . _objectivePrerequisite) inactiveGoals

  suppressHidden =
    if isCheating
      then id
      else filter $ not . _objectiveHidden

  (activeGoals, inactiveGoals) = partitionActiveObjectives objectiveCompletion
