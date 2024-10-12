{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Graph utilities shared by multiple aspects of scenarios
module Swarm.Util.Graph (
  isAcyclicGraph,
  failOnCyclicGraph,
) where

import Control.Monad (forM_)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Util

isAcyclicGraph :: [SCC a] -> Bool
isAcyclicGraph =
  all isAcyclicVertex
 where
  isAcyclicVertex = \case
    AcyclicSCC _ -> True
    _ -> False

getGraphCycles :: [SCC a] -> [[a]]
getGraphCycles =
  mapMaybe getCycle
 where
  getCycle = \case
    AcyclicSCC _ -> Nothing
    CyclicSCC c -> Just c

failOnCyclicGraph ::
  Ord key =>
  Text ->
  (a -> Text) ->
  [(a, key, [key])] ->
  Either Text ()
failOnCyclicGraph graphType keyFunction gEdges =
  forM_ (NE.nonEmpty $ getGraphCycles $ stronglyConnComp gEdges) $ \cycles ->
    Left $
      T.unwords
        [ graphType
        , "graph contains cycles:"
        , commaList $
            NE.toList $
              fmap (brackets . T.intercalate " -> " . fmap keyFunction) cycles
        ]
