{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for performing graph analysis on Objective prerequisites
module Swarm.Game.Scenario.Objective.Graph where

import Control.Arrow ((&&&))
import Control.Lens (view, (^.), (^..))
import Data.Aeson
import Data.BoolExpr (Signed (Positive))
import Data.BoolExpr qualified as BE
import Data.Graph (Graph, SCC, graphFromEdges, stronglyConnComp)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Language.Syntax (SwarmType)
import Swarm.Util.Graph (isAcyclicGraph)

-- | This is only needed for constructing a Graph,
-- which requires all nodes to have a key.
data ObjectiveId
  = Label (Signed ObjectiveLabel)
  | -- | for unlabeled objectives
    Ordinal Int
  deriving (Eq, Ord, Show, Generic, ToJSON)

data GraphInfo phase = GraphInfo
  { actualGraph :: Graph
  , isAcyclic :: Bool
  , sccInfo :: [SCC (Objective phase)]
  , nodeIDs :: [ObjectiveId]
  }
  deriving (Generic)

deriving instance (Show (SwarmType phase)) => ToJSON (GraphInfo phase)

instance (Show (SwarmType phase)) => ToJSON (SCC (Objective phase)) where
  toJSON = String . T.pack . show

instance ToJSON Graph where
  toJSON = String . T.pack . show

instance ToSample (GraphInfo phase) where
  toSamples _ = SD.noSamples

deriving instance Generic (BE.Signed ObjectiveLabel)
deriving instance ToJSON (BE.Signed ObjectiveLabel)

getDistinctConstants :: (Ord a) => Prerequisite a -> Set (BE.Signed a)
getDistinctConstants = Set.fromList . BE.constants . toBoolExpr

-- | Collect all of the constants that have a negation.
-- This is necessary for enumerating all of the distinct
-- nodes when constructing a Graph, as we treat a constant
-- and its negation as distinct nodes.
getNegatedIds :: [Objective phase] -> Map ObjectiveLabel (Objective phase)
getNegatedIds objs =
  M.fromList $ mapMaybe f allConstants
 where
  objectivesById = getObjectivesById objs

  allPrereqExpressions = mapMaybe (view objectivePrerequisite) objs
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

getObjectivesById :: [Objective phase] -> Map ObjectiveLabel (Objective phase)
getObjectivesById objs =
  M.fromList $
    map swap $
      mapMaybe (sequenceA . (id &&& view objectiveId)) objs

-- | Uses the textual labels for those objectives that
-- have them, and assigns arbitrary integer IDs for
-- the remaining.
--
-- Only necessary for constructing a "Graph".
assignIds :: [Objective phase] -> Map ObjectiveId (Objective phase)
assignIds objs =
  unlabeledObjsMap <> labeledObjsMap
 where
  objectivesById = getObjectivesById objs

  labeledObjsMap = M.mapKeys (Label . Positive) objectivesById

  unlabeledObjs = filter (null . view objectiveId) objs
  unlabeledObjsMap = M.fromList $ zipWith (\x y -> (Ordinal x, y)) [0 ..] unlabeledObjs

type Edges phase = [(Objective phase, ObjectiveId, [ObjectiveId])]

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
makeGraph :: Edges phase -> Graph
makeGraph edges =
  myGraph
 where
  (myGraph, _, _) = graphFromEdges edges

makeGraphEdges :: [Objective phase] -> Edges phase
makeGraphEdges objectives =
  rootTuples <> negatedTuples
 where
  rootTuples = map f $ M.toList $ assignIds objectives
  negatedTuples = map gg $ M.toList $ getNegatedIds objectives
  gg (k, v) = (v, Label $ BE.Negative k, [])

  f (k, v) = (v, k, maybe [] (map Label . g) $ v ^. objectivePrerequisite)
  g = Set.toList . getDistinctConstants . logic

makeGraphInfo :: ObjectiveCompletion phase -> GraphInfo phase
makeGraphInfo oc =
  GraphInfo
    (makeGraph edges)
    (isAcyclicGraph connectedComponents)
    connectedComponents
    (M.keys $ assignIds objs)
 where
  edges = makeGraphEdges objs
  connectedComponents = stronglyConnComp edges
  objs = oc ^.. allObjectives
