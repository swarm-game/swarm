{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities for performing graph analysis on Objective prerequisites
module Swarm.Game.Scenario.Objective.Graph where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.BoolExpr (Signed (Positive))
import Data.BoolExpr qualified as BE
import Data.Graph (Graph, SCC (AcyclicSCC), graphFromEdges, stronglyConnComp)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Game.Scenario.Objective.WinCheck

-- | This is only needed for constructing a Graph,
-- which requires all nodes to have a key.
data ObjectiveId
  = Label (Signed ObjectiveLabel)
  | -- | for unlabeled objectives
    Ordinal Int
  deriving (Eq, Ord, Show, Generic, ToJSON)

data GraphInfo = GraphInfo
  { actualGraph :: Graph
  , isAcyclic :: Bool
  , sccInfo :: [SCC Objective]
  , nodeIDs :: [ObjectiveId]
  }
  deriving (Show, Generic, ToJSON)

instance ToJSON (SCC Objective) where
  toJSON = String . T.pack . show

instance ToJSON Graph where
  toJSON = String . T.pack . show

getConstFromSigned :: BE.Signed a -> a
getConstFromSigned = \case
  BE.Positive x -> x
  BE.Negative x -> x

-- | Collect all of the constants that have a negation.
-- This is necessary for enumerating all of the distinct
-- nodes when constructing a Graph, as we treat a constant
-- and its negation as distinct nodes.
getNegatedIds :: [Objective] -> Map ObjectiveLabel Objective
getNegatedIds objs =
  M.fromList $ mapMaybe f allConstants
 where
  objectivesById = getObjectivesById objs

  allPrereqExpressions = mapMaybe _objectivePrerequisite objs
  allConstants =
    mapMaybe onlyNegative
      . Set.toList
      . Set.unions
      . map (getDistinctConstants . logic)
      $ allPrereqExpressions

  f = sequenceA . \x -> (x, M.lookup x objectivesById)

  onlyNegative = \case
    BE.Negative x -> Just x
    _ -> Nothing

getObjectivesById :: [Objective] -> Map ObjectiveLabel Objective
getObjectivesById objs =
  M.fromList $
    map swap $
      mapMaybe (sequenceA . (id &&& _objectiveId)) objs

-- | Uses the textual labels for those objectives that
-- have them, and assigns arbitrary integer IDs for
-- the remaining.
--
-- Only necessary for constructing a "Graph".
assignIds :: [Objective] -> Map ObjectiveId Objective
assignIds objs =
  unlabeledObjsMap <> labeledObjsMap
 where
  objectivesById = getObjectivesById objs

  labeledObjsMap = M.mapKeys (Label . Positive) objectivesById

  unlabeledObjs = filter (null . _objectiveId) objs
  unlabeledObjsMap = M.fromList $ zipWith (\x y -> (Ordinal x, y)) [0 ..] unlabeledObjs

-- | NOTE: Based strictly on the goal labels, the graph could
-- potentially contain a cycle, if there exist
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
makeGraph :: [Objective] -> Graph
makeGraph objectives =
  myGraph
 where
  (myGraph, _, _) = graphFromEdges $ makeGraphEdges objectives

makeGraphEdges :: [Objective] -> [(Objective, ObjectiveId, [ObjectiveId])]
makeGraphEdges objectives =
  rootTuples <> negatedTuples
 where
  rootTuples = map f $ M.toList $ assignIds objectives
  negatedTuples = map gg $ M.toList $ getNegatedIds objectives
  gg (k, v) = (v, Label $ BE.Negative k, [])

  f (k, v) = (v, k, maybe [] (map Label . g) $ _objectivePrerequisite v)
  g = Set.toList . getDistinctConstants . logic

getStronglyConnectedComponents :: [Objective] -> [SCC Objective]
getStronglyConnectedComponents objectives =
  stronglyConnComp $ makeGraphEdges objectives

isAcyclicGraph :: [Objective] -> Bool
isAcyclicGraph objectives =
  all isAcyclicVerex $ getStronglyConnectedComponents objectives
 where
  isAcyclicVerex = \case
    AcyclicSCC _ -> True
    _ -> False

makeGraphInfo :: ObjectiveCompletion -> GraphInfo
makeGraphInfo oc =
  GraphInfo
    (makeGraph objs)
    (isAcyclicGraph objs)
    (getStronglyConnectedComponents objs)
    (M.keys $ assignIds objs)
 where
  objs = listAllObjectives $ completionBuckets oc
