{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities to check whether conditions are met for a game win/loss.
module Swarm.Game.Scenario.Objective.WinCheck where

import Data.Aeson
import Data.BoolExpr qualified as BE
import Data.BoolExpr.Simplify qualified as Simplify
import Data.List (partition)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Logic as L

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

isPrereqsSatisfied :: ObjectiveCompletion -> Objective -> Bool
isPrereqsSatisfied completions =
  maybe True f . _objectivePrerequisite
 where
  f = BE.evalBoolExpr getTruth . L.toBoolExpr . logic

  getTruth :: ObjectiveLabel -> Bool
  getTruth label = Set.member label $ completedIDs completions

isUnwinnablePrereq :: Set ObjectiveLabel -> Prerequisite ObjectiveLabel -> Bool
isUnwinnablePrereq completedObjectives =
  Simplify.cannotBeTrue . Simplify.replace boolMap . L.toBoolExpr
 where
  boolMap =
    M.fromList $
      map (,True) $
        Set.toList completedObjectives

isUnwinnable :: ObjectiveCompletion -> Objective -> Bool
isUnwinnable completions obj =
  maybe False (isUnwinnablePrereq (completedIDs completions) . logic) $ _objectivePrerequisite obj

-- | The first element of the returned tuple consists of "active" objectives,
-- the second element "inactive".
partitionActiveObjectives :: ObjectiveCompletion -> ([Objective], [Objective])
partitionActiveObjectives oc =
  partition (isPrereqsSatisfied oc) $
    incomplete $
      completionBuckets oc

getActiveObjectives :: ObjectiveCompletion -> [Objective]
getActiveObjectives =
  fst . partitionActiveObjectives


deriving instance Generic (BE.Signed ObjectiveLabel)
deriving instance ToJSON (BE.Signed ObjectiveLabel)

-- | For debugging only (via Web API)
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
