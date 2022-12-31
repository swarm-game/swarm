module Swarm.Game.Scenario.Objective.Presentation.Model where

import Swarm.Game.Scenario.Objective
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Aeson
import GHC.Generics (Generic)

-- | These are intended to be used as keys in a map
-- of lists of goals.
data GoalStatus
  = Upcoming
    -- ^ Goals in this category have other goals as prerequisites.
    -- However, they are only displayed if the "previewable" attribute
    -- is `true`.
  | Active
    -- ^ Goals in this category may be pursued in parallel.
    -- However, they are only displayed if the "hidden" attribute
    -- is `false`.
  | Completed
  | Failed
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON, ToJSONKey)

-- | TODO Could also add an "ObjectiveFailed" constructor...
newtype Announcement
  = ObjectiveCompleted Objective
  deriving (Show, Generic, ToJSON)

type CategorizedGoals = Map GoalStatus (NonEmpty Objective)

data GoalDisplay = GoalDisplay {
    announcements :: [Announcement]
  , goals :: CategorizedGoals
  } deriving (Show, Generic, ToJSON)

emptyGoalDisplay :: GoalDisplay
emptyGoalDisplay = GoalDisplay mempty mempty

hasAnythingToShow :: GoalDisplay -> Bool
hasAnythingToShow (GoalDisplay ann g) = not (null ann && null g)

constructGoalMap :: Bool -> ObjectiveCompletion -> CategorizedGoals
constructGoalMap isCheating objectiveCompletion@(ObjectiveCompletion buckets _) =
  M.fromList $
    mapMaybe (traverse nonEmpty) categoryList
  where
  categoryList = [
      (Upcoming, displayableInactives)
    , (Active, suppressHidden activeGoals)
    , (Completed, completed buckets)
    , (Failed, unwinnable buckets)
    ]

  displayableInactives = suppressHidden $
    filter (maybe False previewable . _objectivePrerequisite) inactiveGoals

  suppressHidden = if isCheating
    then id
    else filter $ not . _objectiveHidden

  (activeGoals, inactiveGoals) = partitionActiveObjectives objectiveCompletion