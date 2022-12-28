{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Scenario.Objective where

import Control.Arrow ((&&&))
import Control.Lens hiding (from, (<.>))
import Control.Monad (unless)
import Data.Aeson
import Data.BoolExpr (evalBoolExpr)
import Data.Foldable (foldl', for_, toList)
import Data.Graph (graphFromEdges)
import Data.List (partition)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Logic as L
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Util (quote, reflow)
import Witch (into)

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

-- | TODO: Should use a "smart constructor"
-- on this so that all ObjectId references
-- within the 'completionBuckets' list are guaranteed
-- to exist as keys of the 'byId' map.
-- Or, perhaps hide the implementation detail
-- of there being a map, and just define an
-- accessor function that does not fail?
data ObjectiveLookup = ObjectiveLookup
  { byId :: Map ObjectiveLabel Objective
  , completionBuckets :: CompletionBuckets
  -- ^ This is the authoritative "completion status"
  -- for all objectives.
  -- Note that there is a separate Map to store the
  -- completion status of prerequisite objectives.
  -- Those prerequisite objectives are required to have
  -- labels, but other objectives are not.
  -- Therefore only prerequisites exist in the completion
  -- map keyed by label.
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

listAllObjectives :: CompletionBuckets -> [Objective]
listAllObjectives (CompletionBuckets x y) = x <> y

data ObjectiveCompletion = ObjectiveCompletion
  { objectiveLookup :: ObjectiveLookup
  , completedIDs :: Set.Set ObjectiveLabel
  }
  deriving (Show, Generic, FromJSON, ToJSON)

addCompleted :: ObjectiveCompletion -> Objective -> ObjectiveCompletion
addCompleted (ObjectiveCompletion (ObjectiveLookup objsById buckets) cmplById) obj =
  ObjectiveCompletion newObjLookup newCmplById
 where
  newBuckets =
    buckets
      { completed = obj : completed buckets
      }
  newObjLookup = ObjectiveLookup objsById newBuckets
  newCmplById = case _objectiveId obj of
    Nothing -> cmplById
    Just lbl -> Set.insert lbl cmplById

setIncomplete :: ([Objective] -> [Objective]) -> ObjectiveCompletion -> ObjectiveCompletion
setIncomplete f (ObjectiveCompletion (ObjectiveLookup objsById buckets) cmplById) =
  ObjectiveCompletion newObjLookup cmplById
 where
  newBuckets =
    buckets
      { incomplete = f $ incomplete buckets
      }
  newObjLookup = ObjectiveLookup objsById newBuckets

addIncomplete :: ObjectiveCompletion -> Objective -> ObjectiveCompletion
addIncomplete oc obj =
  setIncomplete (obj :) oc

isPrereqsSatisfied :: ObjectiveCompletion -> Objective -> Bool
isPrereqsSatisfied completions obj = case _objectivePrerequisite obj of
  Nothing -> True
  Just prereqs -> evalBoolExpr getTruth $ L.toBoolExpr prereqs
 where
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
        completionBuckets $
          objectiveLookup oc

-- | For debugging only
data PrereqSatisfaction = PrereqSatisfaction
  { objective :: Objective
  , prereqsSatisfied :: Bool
  }
  deriving (Generic, ToJSON)

getSatisfaction :: ObjectiveCompletion -> [PrereqSatisfaction]
getSatisfaction x =
  map f $
    listAllObjectives $
      completionBuckets $
        objectiveLookup x
 where
  f y = PrereqSatisfaction y (isPrereqsSatisfied x y)

-- | Uses the textual labels for those objectives that
-- have them, and assigns arbitrary integer IDs for
-- the remaining.
assignIds :: [Objective] -> Map ObjectiveId Objective
assignIds =
  M.fromList . fst . foldl' f ([], 0)
 where
  f (tuples, currentCount) obj = case _objectiveId obj of
    Just n -> ((Label n, obj) : tuples, currentCount)
    Nothing -> ((Ordinal currentCount, obj) : tuples, currentCount + 1)

processObjectives ::
  MonadFail m =>
  [Objective] ->
  m ObjectiveLookup
processObjectives objectives = do
  for_ objectives $ \x -> case _objectivePrerequisite x of
    Just p ->
      unless (null remaining) $
        fail . into @String $
          T.unwords
            [ "Reference to undefined objective(s)"
            , T.intercalate ", " (map quote $ Set.toList remaining) <> "."
            , "Defined are:"
            , T.intercalate ", " (map quote $ Set.toList allIds)
            ]
     where
      refs = Set.fromList $ toList p
      remaining = Set.difference refs allIds
    Nothing -> return ()

  return objectivesLookup
 where
  objectivesById =
    M.fromList $
      map swap $
        mapMaybe (sequenceA . (id &&& _objectiveId)) objectives
  allIds = M.keysSet objectivesById
  objectivesLookup = ObjectiveLookup objectivesById $ CompletionBuckets objectives []

  -- NOTE: Based solely on goal labels, the graph could potentially contain a cycle,
  -- if there exist
  -- mutually-exclusive goals. That is, if goal A depends on the NOT
  -- of "goal B".  Goal B could then also depend on "NOT Goal A" (re-enforcing the
  -- mutual-exclusivity), or it could mandate a completion order, e.g.:
  -- Goal A and Goal B are simultaneously available to pursue.  However, if the
  -- player completes Goal B first, then it closes off the option to complete
  -- Goal A.  However, if Goal A is completed first, then the user is also allowed
  -- to complete Goal B.
  --
  -- To avoid a "cycle" in this circumstance, "A" needs to exist as a distinct node
  -- from "NOT A" in the graph.
  f (k, v) = (v, k, maybe [] (map Label . toList) $ _objectivePrerequisite v)
  myGraph = graphFromEdges $ map f $ M.toList $ assignIds objectives
