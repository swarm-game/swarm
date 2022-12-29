{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Swarm.Game.Scenario.Objective.Graph where

import Control.Arrow ((&&&))
import Data.Aeson
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

data GraphInfo = GraphInfo
  { actualGraph :: Graph
  , isAcyclic :: Bool
  , sccInfo :: [SCC Objective]
  }
  deriving (Eq, Show, Generic, ToJSON)

instance ToJSON (SCC Objective) where
  toJSON = String . T.pack . show

instance ToJSON Graph where
  toJSON = String . T.pack . show

-- | Uses the textual labels for those objectives that
-- have them, and assigns arbitrary integer IDs for
-- the remaining.
--
-- Only necessary for constructing a "Graph".
assignIds :: [Objective] -> Map ObjectiveId Objective
assignIds objs =
  unlabeledObjsMap <> labeledObjsMap
 where
  objectivesById =
    M.fromList $
      map swap $
        mapMaybe (sequenceA . (id &&& _objectiveId)) objs

  allPrereqExpressions = mapMaybe _objectivePrerequisite objs
  allConstants = Set.unions $ map getDistinctConstants allPrereqExpressions
  labeledObjsMap = M.fromList $ mapMaybe f $ Set.toList allConstants
  f = sequenceA . \x -> (Label x, M.lookup (getConstFromSigned x) objectivesById)

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
  map f $ M.toList $ assignIds objectives
 where
  f (k, v) = (v, k, maybe [] (map Label . g) $ _objectivePrerequisite v)
  g = Set.toList . getDistinctConstants

getStronglyConnectedComponents :: [Objective] -> [SCC Objective]
getStronglyConnectedComponents objectives =
  stronglyConnComp $ makeGraphEdges objectives

isAcyclicGraph :: [Objective] -> Bool
isAcyclicGraph objectives =
  all isSingleVertex $ getStronglyConnectedComponents objectives
 where
  isSingleVertex = \case
    AcyclicSCC _ -> True
    _ -> False

makeGraphInfo :: ObjectiveCompletion -> GraphInfo
makeGraphInfo oc =
  GraphInfo
    (makeGraph objs)
    (isAcyclicGraph objs)
    (getStronglyConnectedComponents objs)
 where
  objs = listAllObjectives $ completionBuckets oc
