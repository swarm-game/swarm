{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A UI-centric model for Objective presentation.
module Swarm.TUI.Model.Goal where

import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens (makeLenses, view, (^..))
import Data.Aeson
import Data.List.Extra (enumerate)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.WinCheck
import Swarm.TUI.Model.Name

-- | These are intended to be used as keys in a map
-- of lists of goals.
data GoalStatus
  = -- | Goals in this category have other goals as prerequisites.
    -- However, they are only displayed if the "previewable" attribute
    -- is @true@.
    Upcoming
  | -- | Goals in this category may be pursued in parallel.
    -- However, they are only displayed if the "hidden" attribute
    -- is @false@.
    Active
  | -- | A goal's programmatic condition, as well as all its prerequisites, were completed.
    -- This is a "latch" mechanism; at some point the conditions required to meet the goal may
    -- no longer hold. Nonetheless, the goal remains "completed".
    Completed
  | -- | A goal that can no longer be achieved.
    -- If this goal is not an "optional" goal, then the player
    -- also "loses" the scenario.
    --
    -- Note that currently the only way to "fail" a goal is by way
    -- of a negative prerequisite that was completed.
    Failed
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON, ToJSONKey)

type CategorizedGoals = Map GoalStatus (NonEmpty Objective)

data GoalEntry
  = Header GoalStatus
  | Goal GoalStatus Objective
  | Spacer

shouldSkipSelection :: GoalEntry -> Bool
shouldSkipSelection = \case
  Goal _ _ -> False
  _ -> True

data GoalTracking = GoalTracking
  { announcements :: [Announcement]
  -- ^ TODO: #1044 the actual contents of these are not used yet,
  -- other than as a flag to pop up the Goal dialog.
  , goals :: CategorizedGoals
  }
  deriving (Generic, ToJSON)

instance ToSample GoalTracking where
  toSamples _ =
    SD.samples
      [ GoalTracking mempty mempty
      -- TODO: #1552 add simple objective sample
      ]

data GoalDisplay = GoalDisplay
  { _goalsContent :: GoalTracking
  , _listWidget :: BL.List Name GoalEntry
  -- ^ required for maintaining the selection/navigation
  -- state among list items
  , _focus :: FocusRing Name
  }

makeLenses ''GoalDisplay

emptyGoalDisplay :: GoalDisplay
emptyGoalDisplay =
  GoalDisplay
    (GoalTracking mempty mempty)
    (BL.list (GoalWidgets ObjectivesList) mempty 1)
    (focusRing $ map GoalWidgets enumerate)

hasAnythingToShow :: GoalTracking -> Bool
hasAnythingToShow (GoalTracking ann g) = not (null ann && null g)

hasMultipleGoals :: GoalTracking -> Bool
hasMultipleGoals gt =
  goalCount > 1
 where
  goalCount = sum . M.elems . M.map NE.length . goals $ gt

constructGoalMap :: Bool -> ObjectiveCompletion -> CategorizedGoals
constructGoalMap showHidden oc =
  M.fromList $
    mapMaybe (traverse nonEmpty) categoryList
 where
  categoryList =
    [ (Upcoming, displayableInactives)
    , (Active, suppressHidden activeGoals)
    , (Completed, oc ^.. completedObjectives)
    , (Failed, oc ^.. unwinnableObjectives)
    ]

  displayableInactives =
    suppressHidden $
      filter (maybe False previewable . view objectivePrerequisite) inactiveGoals

  suppressHidden =
    if showHidden
      then id
      else filter $ not . view objectiveHidden

  (activeGoals, inactiveGoals) = partitionActiveObjectives oc
