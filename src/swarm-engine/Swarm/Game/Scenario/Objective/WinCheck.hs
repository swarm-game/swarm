{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities to check whether conditions are met for a game win/loss.
module Swarm.Game.Scenario.Objective.WinCheck where

import Control.Lens (andOf, view, (^.), (^..))
import Data.Aeson (ToJSON)
import Data.BoolExpr qualified as BE
import Data.BoolExpr.Simplify qualified as Simplify
import Data.List (partition)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph (getDistinctConstants)
import Swarm.Game.Scenario.Objective.Logic as L
import Swarm.Language.Syntax (Phase (..))
import Swarm.Util.Lens (concatFold)

-- | We have "won" if all of the "unwinnable" or remaining "incomplete" objectives are "optional".
didWin :: ObjectiveCompletion phase -> Bool
didWin = andOf ((incompleteObjectives `concatFold` unwinnableObjectives) . objectiveOptional)

-- | We have "lost" if any of the "unwinnable" objectives are not "optional".
didLose :: ObjectiveCompletion phase -> Bool
didLose = not . andOf (unwinnableObjectives . objectiveOptional)

isPrereqsSatisfied :: ObjectiveCompletion phase -> Objective phase -> Bool
isPrereqsSatisfied completions =
  maybe True f . view objectivePrerequisite
 where
  f = BE.evalBoolExpr getTruth . L.toBoolExpr . logic

  getTruth :: ObjectiveLabel -> Bool
  getTruth label = Set.member label $ completions ^. completedIDs

isUnwinnablePrereq :: Set ObjectiveLabel -> Prerequisite ObjectiveLabel -> Bool
isUnwinnablePrereq completed =
  Simplify.cannotBeTrue . Simplify.replace boolMap . L.toBoolExpr
 where
  boolMap = M.fromList . map (,True) . Set.toList $ completed

isUnwinnable :: ObjectiveCompletion phase -> Objective phase -> Bool
isUnwinnable completions obj =
  maybe False (isUnwinnablePrereq (completions ^. completedIDs) . logic) $ obj ^. objectivePrerequisite

-- | The first element of the returned tuple consists of "active" objectives,
-- the second element "inactive".
partitionActiveObjectives :: ObjectiveCompletion phase -> ([Objective phase], [Objective phase])
partitionActiveObjectives oc =
  partition (isPrereqsSatisfied oc) $ oc ^.. incompleteObjectives

getActiveObjectives :: ObjectiveCompletion phase -> [Objective phase]
getActiveObjectives =
  fst . partitionActiveObjectives

-- | For debugging only (via Web API)
data PrereqSatisfaction = PrereqSatisfaction
  { objective :: Objective Typed
  , deps :: Set (BE.Signed ObjectiveLabel)
  , prereqsSatisfied :: Bool
  }
  deriving (Generic, ToJSON)

instance ToSample PrereqSatisfaction where
  toSamples _ = SD.noSamples

-- | Used only by the web interface for debugging
getSatisfaction :: ObjectiveCompletion Typed -> [PrereqSatisfaction]
getSatisfaction oc = map f $ oc ^.. allObjectives
 where
  f y =
    PrereqSatisfaction
      y
      (maybe mempty (getDistinctConstants . logic) $ y ^. objectivePrerequisite)
      (isPrereqsSatisfied oc y)
