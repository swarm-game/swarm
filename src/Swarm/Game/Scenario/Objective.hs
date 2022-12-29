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
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (reflow)

------------------------------------------------------------
-- Scenario objectives
------------------------------------------------------------

-- | An objective is a condition to be achieved by a player in a
--   scenario.
data Objective = Objective
  { _objectiveGoal :: [Text]
  , _objectiveCondition :: ProcessedTerm
  , _objectiveId :: Maybe ObjectiveLabel
  , _objectiveOptional :: Bool
  , _objectivePrerequisite :: Maybe (Prerequisite ObjectiveLabel)
  }
  deriving (Eq, Show, Generic, ToJSON)

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

-- | Boolean expression the represents the condition dependencies which also
-- must have been evaluated to True.
-- Note that the achievement of these objective dependencies is
-- persistent; once achieved, it still counts even if the "condition"
-- might not still hold. The condition is never re-evaluated once True.
objectivePrerequisite :: Lens' Objective (Maybe (Prerequisite ObjectiveLabel))

instance FromJSON Objective where
  parseJSON = withObject "objective" $ \v ->
    Objective
      <$> (fmap . map) reflow (v .:? "goal" .!= [])
      <*> (v .: "condition")
      <*> (v .:? "id")
      <*> (v .:? "optional" .!= False)
      <*> (v .:? "prerequisite")

data CompletionBuckets = CompletionBuckets
  { incomplete :: [Objective]
  , completed :: [Objective]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Concatenates all incomplete and completed objectives.
listAllObjectives :: CompletionBuckets -> [Objective]
listAllObjectives (CompletionBuckets x y) = x <> y

-- | We have "won" if all of the remaining incomplete objectives are "optional".
didWin :: ObjectiveCompletion -> Bool
didWin = all _objectiveOptional . incomplete . completionBuckets

addCompleted :: ObjectiveCompletion -> Objective -> ObjectiveCompletion
addCompleted (ObjectiveCompletion buckets cmplIds) obj =
  ObjectiveCompletion newBuckets newCmplById
 where
  newBuckets =
    buckets
      { completed = obj : completed buckets
      }
  newCmplById = case _objectiveId obj of
    Nothing -> cmplIds
    Just lbl -> Set.insert lbl cmplIds

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
  f = BE.evalBoolExpr getTruth . L.toBoolExpr

  getTruth :: ObjectiveLabel -> Bool
  getTruth label = Set.member label $ completedIDs completions

-- | TODO: Do we need this? Explain why in this comment.
getActiveObjectives :: ObjectiveCompletion -> [Objective]
getActiveObjectives oc =
  activeObjectives
 where
  (activeObjectives, _inactiveObjectives) =
    partition (isPrereqsSatisfied oc) $
      incomplete $
        completionBuckets oc

-- | For debugging only
data PrereqSatisfaction = PrereqSatisfaction
  { objective :: Objective
  , deps :: Set (BE.Signed ObjectiveLabel)
  , prereqsSatisfied :: Bool
  }
  deriving (Generic, ToJSON)

instance ToJSON (BE.Signed ObjectiveLabel) where
  toJSON = String . T.pack . show

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
      (maybe mempty getDistinctConstants $ _objectivePrerequisite y)
      (isPrereqsSatisfied oc y)

getDistinctConstants :: (Ord a) => Prerequisite a -> Set (BE.Signed a)
getDistinctConstants = Set.fromList . BE.constants . toBoolExpr

getConstFromSigned :: BE.Signed a -> a
getConstFromSigned = \case
  BE.Positive x -> x
  BE.Negative x -> x
