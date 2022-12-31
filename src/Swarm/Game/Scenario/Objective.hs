{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Swarm.Game.Scenario.Objective where

import Control.Lens hiding (from, (<.>))
import Data.Aeson
import Data.BoolExpr qualified as BE
import Data.List (partition)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map qualified as M
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (reflow)
import Swarm.TUI.Model.Achievement.Definitions
import Data.BoolExpr.Simplify qualified as Simplify

------------------------------------------------------------
-- Scenario objectives
------------------------------------------------------------

data PrerequisiteConfig = PrerequisiteConfig {
    previewable :: Bool
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
    -- ^ Boolean expression the represents the condition dependencies which also
    -- must have been evaluated to True.
    -- Note that the achievement of these objective dependencies is
    -- persistent; once achieved, it still counts even if the "condition"
    -- might not still hold. The condition is never re-evaluated once True.
  } deriving (Eq, Show, Generic, ToJSON)


instance FromJSON PrerequisiteConfig where
  parseJSON = withObject "prerequisite" $ \v ->
    PrerequisiteConfig
      <$> (v .:? "previewable" .!= False)
      <*> (v .: "logic")

-- | An objective is a condition to be achieved by a player in a
--   scenario.
data Objective = Objective
  { _objectiveGoal :: [Text]
  , _objectiveCondition :: ProcessedTerm
  , _objectiveId :: Maybe ObjectiveLabel
  , _objectiveOptional :: Bool
  , _objectivePrerequisite :: Maybe PrerequisiteConfig
  , _objectiveHidden :: Bool
  , _objectiveAchievement :: Maybe AchievementInfo
  }
  deriving (Show, Generic, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''Objective

-- | An explanation of the goal of the objective, shown to the player
--   during play.  It is represented as a list of paragraphs.
objectiveGoal :: Lens' Objective [Text]

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
      <$> (fmap . map) reflow (v .:? "goal" .!= [])
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

-- | We have "won" if all of the "unwinnable" or remaining "incomplete" objectives are "optional".
didWin :: ObjectiveCompletion -> Bool
didWin oc = all _objectiveOptional $ incomplete buckets <> unwinnable buckets
  where
    buckets = completionBuckets oc

-- | We have "lost" if any of the "unwinnable" objectives not "optional".
didLose :: ObjectiveCompletion -> Bool
didLose oc = not $ all _objectiveOptional $ unwinnable buckets
  where
    buckets = completionBuckets oc

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
  ObjectiveCompletion newBuckets newCmplById
 where
  newBuckets =
    buckets
      { unwinnable = obj : unwinnable buckets
      }
  -- TODO Should we also have a set that represents the unwinnable?
  newCmplById = cmplIds
  -- newCmplById = case _objectiveId obj of
  --   Nothing -> cmplIds
  --   Just lbl -> Set.insert lbl cmplIds

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

isPrereqsSatisfied :: ObjectiveCompletion -> Objective -> Bool
isPrereqsSatisfied completions =
  maybe True f . _objectivePrerequisite
 where
  f = BE.evalBoolExpr getTruth . L.toBoolExpr . logic

  getTruth :: ObjectiveLabel -> Bool
  getTruth label = Set.member label $ completedIDs completions

isUnwinnable :: ObjectiveCompletion -> Objective -> Bool
isUnwinnable completions obj =
  maybe False f $ _objectivePrerequisite obj
 where
  f = Simplify.cannotBeTrue . Simplify.replace boolMap . L.toBoolExpr . logic

  boolMap = M.fromList $ map (, True) $
    Set.toList $ completedIDs completions

partitionActiveObjectives :: ObjectiveCompletion -> ([Objective], [Objective])
partitionActiveObjectives oc =
  partition (isPrereqsSatisfied oc) $
    incomplete $
      completionBuckets oc

getActiveObjectives :: ObjectiveCompletion -> [Objective]
getActiveObjectives =
  fst . partitionActiveObjectives

-- | For debugging only
data PrereqSatisfaction = PrereqSatisfaction
  { objective :: Objective
  , deps :: Set (BE.Signed ObjectiveLabel)
  , prereqsSatisfied :: Bool
  }
  deriving (Generic, ToJSON)

-- | Used only by the web interface for debugging
getSatisfaction :: ObjectiveCompletion -> [PrereqSatisfaction]
getSatisfaction oc =
  map f $
    listAllObjectives $
      completionBuckets oc
 where
  f y =
    PrereqSatisfaction
      y
      (maybe mempty (getDistinctConstants . logic) $ _objectivePrerequisite y)
      (isPrereqsSatisfied oc y)

getDistinctConstants :: (Ord a) => Prerequisite a -> Set (BE.Signed a)
getDistinctConstants = Set.fromList . BE.constants . toBoolExpr
