{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Swarm.Game.Scenario.Objective where

import Control.Applicative ((<|>))
import Control.Lens hiding (from, (<.>))
import Data.Aeson
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Markdown qualified as Markdown
import GHC.Generics (Generic)
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Language.Pipeline (ProcessedTerm)

------------------------------------------------------------
-- Scenario objectives
------------------------------------------------------------

data PrerequisiteConfig = PrerequisiteConfig
  { previewable :: Bool
  -- ^ Typically, only the currently "active" objectives are
  -- displayed to the user in the Goals dialog. An objective
  -- is "active" if all of its prerequisites are met.
  --
  -- However, some objectives may be "high-level", in that they may
  -- explain the broader intention behind potentially multiple
  -- prerequisites.
  --
  -- Set this to option True to display this goal in the "upcoming" section even
  -- if the objective has currently unmet prerequisites.
  , logic :: Prerequisite ObjectiveLabel
  -- ^ Boolean expression of dependencies upon other objectives. Variables in this expression
  -- are the "id"s of other objectives, and become "true" if the corresponding objective is completed.
  -- The "condition" of the objective at hand shall not be evaluated until its
  -- prerequisite expression evaluates as True.
  --
  -- Note that the achievement of these objective dependencies is
  -- persistent; once achieved, they still count even if their "condition"
  -- might not still hold. The condition is never re-evaluated once True.
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON PrerequisiteConfig where
  -- Parsing JSON/YAML 'PrerequisiteConfig' has a shorthand option
  -- in which the boolean expression can be written directly,
  -- bypassing the "logic" key.
  -- Furthermore, an "Id" in a boolean expressions can be written
  -- as a bare string without needing the "id" key.
  parseJSON val = preLogic val <|> preObject val
   where
    preObject = withObject "prerequisite" $ \v ->
      PrerequisiteConfig
        <$> (v .:? "previewable" .!= False)
        <*> v .: "logic"
    preLogic = fmap (PrerequisiteConfig False) . parseJSON

-- | An objective is a condition to be achieved by a player in a
--   scenario.
data Objective = Objective
  { _objectiveGoal :: [Markdown.Node]
  , _objectiveTeaser :: Maybe Text
  , _objectiveCondition :: ProcessedTerm
  , _objectiveId :: Maybe ObjectiveLabel
  , _objectiveOptional :: Bool
  , _objectivePrerequisite :: Maybe PrerequisiteConfig
  , _objectiveHidden :: Bool
  , _objectiveAchievement :: Maybe AchievementInfo
  }
  deriving (Eq, Show, Generic, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''Objective

-- | An explanation of the goal of the objective, shown to the player
--   during play.  It is represented as a list of paragraphs.
objectiveGoal :: Lens' Objective [Markdown.Node]

-- | A very short (3-5 words) description of the goal for
-- displaying on the left side of the Objectives modal.
objectiveTeaser :: Lens' Objective (Maybe Text)

-- | A winning condition for the objective, expressed as a
--   program of type @cmd bool@.  By default, this program will be
--   run to completion every tick (the usual limits on the number
--   of CESK steps per tick do not apply).
objectiveCondition :: Lens' Objective ProcessedTerm

-- | Optional name by which this objective may be referenced
-- as a prerequisite for other objectives.
objectiveId :: Lens' Objective (Maybe Text)

-- | Indicates whether the objective is not required in order
-- to "win" the scenario. Useful for (potentially hidden) achievements.
-- If the field is not supplied, it defaults to False (i.e. the
-- objective is mandatory to "win").
objectiveOptional :: Lens' Objective Bool

-- | Dependencies upon other objectives
objectivePrerequisite :: Lens' Objective (Maybe PrerequisiteConfig)

-- | Whether the goal is displayed in the UI before completion.
-- The goal will always be revealed after it is completed.
--
-- This attribute often goes along with an Achievement.
objectiveHidden :: Lens' Objective Bool

-- | An optional Achievement that is to be registered globally
-- when this objective is completed.
objectiveAchievement :: Lens' Objective (Maybe AchievementInfo)

instance FromJSON Objective where
  parseJSON = withObject "objective" $ \v ->
    Objective
      <$> (v .:? "goal" .!= [])
      <*> (v .:? "teaser")
      <*> (v .: "condition")
      <*> (v .:? "id")
      <*> (v .:? "optional" .!= False)
      <*> (v .:? "prerequisite")
      <*> (v .:? "hidden" .!= False)
      <*> (v .:? "achievement")

data CompletionBuckets = CompletionBuckets
  { incomplete :: [Objective]
  , completed :: [Objective]
  , unwinnable :: [Objective]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | TODO: #1044 Could also add an "ObjectiveFailed" constructor...
newtype Announcement
  = ObjectiveCompleted Objective
  deriving (Show, Generic, ToJSON)

data ObjectiveCompletion = ObjectiveCompletion
  { completionBuckets :: CompletionBuckets
  -- ^ This is the authoritative "completion status"
  -- for all objectives.
  -- Note that there is a separate Set to store the
  -- completion status of prerequisite objectives, which
  -- must be carefully kept in sync with this.
  -- Those prerequisite objectives are required to have
  -- labels, but other objectives are not.
  -- Therefore only prerequisites exist in the completion
  -- map keyed by label.
  , completedIDs :: Set.Set ObjectiveLabel
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Concatenates all incomplete and completed objectives.
listAllObjectives :: CompletionBuckets -> [Objective]
listAllObjectives (CompletionBuckets x y z) = x <> y <> z

addCompleted :: Objective -> ObjectiveCompletion -> ObjectiveCompletion
addCompleted obj (ObjectiveCompletion buckets cmplIds) =
  ObjectiveCompletion newBuckets newCmplById
 where
  newBuckets =
    buckets
      { completed = obj : completed buckets
      }
  newCmplById = case _objectiveId obj of
    Nothing -> cmplIds
    Just lbl -> Set.insert lbl cmplIds

addUnwinnable :: Objective -> ObjectiveCompletion -> ObjectiveCompletion
addUnwinnable obj (ObjectiveCompletion buckets cmplIds) =
  ObjectiveCompletion newBuckets cmplIds
 where
  newBuckets =
    buckets
      { unwinnable = obj : unwinnable buckets
      }

setIncomplete ::
  ([Objective] -> [Objective]) ->
  ObjectiveCompletion ->
  ObjectiveCompletion
setIncomplete f (ObjectiveCompletion buckets cmplIds) =
  ObjectiveCompletion newBuckets cmplIds
 where
  newBuckets =
    buckets
      { incomplete = f $ incomplete buckets
      }

addIncomplete :: Objective -> ObjectiveCompletion -> ObjectiveCompletion
addIncomplete obj = setIncomplete (obj :)

-- | Returns the "ObjectiveCompletion" with the "incomplete" goals
-- extracted to a separate tuple member.
-- This is intended as input to a "fold".
extractIncomplete :: ObjectiveCompletion -> (ObjectiveCompletion, [Objective])
extractIncomplete oc =
  (withoutIncomplete, incompleteGoals)
 where
  incompleteGoals = incomplete $ completionBuckets oc
  withoutIncomplete = setIncomplete (const []) oc
