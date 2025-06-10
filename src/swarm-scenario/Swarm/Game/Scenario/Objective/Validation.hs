{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Validity checking for 'Objective' prerequisites
module Swarm.Game.Scenario.Objective.Validation where

import Control.Lens (view, (^.))
import Control.Monad (forM_, unless)
import Data.Foldable (for_, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph
import Swarm.Language.Phase (Phase (Raw))
import Swarm.Util (failT, quote)
import Swarm.Util.Graph

-- | Performs monadic validation before returning
-- the "pure" construction of a wrapper record.
-- This validation entails:
--
-- 1. Ensuring that all goal references utilized in prerequisites
--    actually exist
-- 2. Ensuring that the graph of dependencies is acyclic.
validateObjectives ::
  MonadFail m =>
  [Objective Raw] ->
  m [Objective Raw]
validateObjectives objectives = do
  for_ objectives $ \x -> forM_ (x ^. objectivePrerequisite) $ \p ->
    let refs = Set.fromList $ toList $ logic p
        remaining = Set.difference refs allIds
     in unless (null remaining) $
          failT
            [ "Reference to undefined objective(s)"
            , T.intercalate ", " (map quote $ Set.toList remaining) <> "."
            , "Defined are:"
            , T.intercalate ", " (map quote $ Set.toList allIds)
            ]

  either (fail . T.unpack) return $
    failOnCyclicGraph "Prerequisites" (fromMaybe "N/A" . view objectiveId) edges

  return objectives
 where
  edges = makeGraphEdges objectives
  allIds = Set.fromList $ mapMaybe (view objectiveId) objectives
