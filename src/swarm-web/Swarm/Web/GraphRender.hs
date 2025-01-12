{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Web.GraphRender where

import Control.Lens ((^.), (^..))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph
import Text.Dot

renderGoalsGraph :: ObjectiveCompletion -> String
renderGoalsGraph oc =
  showDot nlg
 where
  edgeLookup = M.fromList $ map (\x@(_, b, _) -> (b, x)) edges
  nlg =
    netlistGraph
      (\k -> maybe mempty (\(a, _, _) -> [("label", T.unpack $ fromMaybe "<?>" $ a ^. objectiveId)]) $ M.lookup k edgeLookup)
      (\k -> maybe mempty (\(_, _, c) -> c) $ M.lookup k edgeLookup)
      ([(a, a) | (_, a, _) <- edges])

  edges = makeGraphEdges objs
  objs = oc ^.. allObjectives
