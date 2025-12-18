{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Graph utilities shared by multiple aspects of scenarios
module Swarm.Util.Graph (
  isAcyclicGraph,
  findCycle,
  failOnCyclicGraph,
) where

import Control.Monad (forM_)
import Control.Monad.Trans.State (State, evalState, gets, modify)
import Data.Graph (SCC (..))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
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

-- | Keep track of the current search path in a DFS, both as a set of
--   vertices (for fast membership testing) and as a reversed list of
--   vertices visited along the current path, in order.
--
--   Note this is different than just keeping track of which vertices
--   have been visited at all; visited vertices remain visited when
--   DFS backtracks, but the DFSPath gets shorter again.
data DFSPath v = DFSPath (Set v) [v]

emptyDFSPath :: DFSPath v
emptyDFSPath = DFSPath S.empty []

appendPath :: Ord v => DFSPath v -> v -> DFSPath v
appendPath (DFSPath s p) v = DFSPath (S.insert v s) (v : p)

-- | Find a cycle in a directed graph (if any exist) via DFS. Returns
--   an ordered list of vertices in the cycle, repeating the first and
--   last vertex.
--
-- >>> findCycle [("a", ["a"])]
-- Just ["a","a"]
-- >>> findCycle [("a", ["b"]), ("b", [])]
-- Nothing
-- >>> findCycle [("a", ["b"]), ("b", ["a"])]
-- Just ["a","b","a"]
-- >>> findCycle [("a", ["b"]), ("b", ["c"]), ("c", ["b"])]
-- Just ["b","c","b"]
-- >>> findCycle [("a",["b"]), ("b",["d","a"]), ("c",["b"]), ("d",[])]
-- Just ["a","b","a"]
-- >>> findCycle [("a",[]), ("b",["d","a"]), ("c",["b"]), ("d",[])]
-- Nothing
-- >>> findCycle [("a",["b"]), ("b",["d","a"]), ("c",["b"]), ("d",["c"])]
-- Just ["b","d","c","b"]
findCycle :: Ord v => [(v, [v])] -> Maybe [v]
findCycle g = findCycleImplicit (map fst g) (fromMaybe [] . (neighbors M.!?))
 where
  neighbors = M.fromList g

-- | A more generic version of 'findCycle' which takes as input an
--   implicit graph description: a list of vertices, a function
--   mapping vertices to labels, and a function mapping each vertex to
--   its (outgoing) neighbors.
findCycleImplicit :: forall v. Ord v => [v] -> (v -> [v]) -> Maybe [v]
findCycleImplicit vertices neighbors = flip evalState S.empty $ dfsL emptyDFSPath vertices
 where
  dfsL :: DFSPath v -> [v] -> State (Set v) (Maybe [v])
  dfsL _ [] = pure Nothing
  dfsL path (v : vs) = do
    found <- dfs path v
    case found of
      Nothing -> dfsL path vs
      Just cyc -> pure (Just cyc)

  dfs :: DFSPath v -> v -> State (Set v) (Maybe [v])
  dfs p@(DFSPath pathMembers path) v
    | v `S.member` pathMembers = pure . Just . (v :) . reverse . (v :) $ takeWhile (/= v) path
    | otherwise = do
        vis <- gets (S.member v)
        case vis of
          True -> pure Nothing
          False -> do
            modify (S.insert v)
            dfsL (appendPath p v) (neighbors v)

-- | Convenience function which checks a given graph for directed
--   cycles, and outputs a custom error message if a directed cycle is
--   found, using the given name for the graph and a method for
--   displaying vertices.
failOnCyclicGraph ::
  Ord v =>
  Text ->
  (v -> Text) ->
  [(v, [v])] ->
  Either Text ()
failOnCyclicGraph graphType keyFunction gEdges =
  forM_ (findCycle gEdges) $ \cyc ->
    Left $
      T.unwords
        [ graphType
        , "graph contains a cycle:"
        , brackets . T.intercalate " -> " . fmap keyFunction $ cyc
        ]
