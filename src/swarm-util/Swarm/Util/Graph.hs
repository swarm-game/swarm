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
import Control.Monad.ST
import Control.Monad.Trans.State (State, evalState, gets, modify)
import Data.Array ((!))
import Data.Array.ST
import Data.Graph (SCC (..), Vertex, graphFromEdges)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Map qualified as M
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

-- | Find a cycle in a directed graph (if any exist) via DFS.
--
-- >>> findCycle [("a", 0, [0])]
-- Just ["a","a"]
-- >>> findCycle [("a", 0, [1]), ("b", 1, [])]
-- Nothing
-- >>> findCycle [("a", 0, [1]), ("b", 1, [0])]
-- Just ["a","b","a"]
-- >>> findCycle [("a", 0, [1]), ("b", 1, [2]), ("c", 2, [1])]
-- Just ["b","c","b"]
-- >>> findCycle [("a",3,[1]), ("b",1,[0,3]), ("c",2,[1]), ("d",0,[])]
-- Just ["b","a","b"]
-- >>> findCycle [("a",3,[]), ("b",1,[0,3]), ("c",2,[1]), ("d",0,[])]
-- Nothing
-- >>> findCycle [("a",3,[1]), ("b",1,[0,3]), ("c",2,[1]), ("d",0,[2])]
-- Just ["d","c","b","d"]
findCycle :: Ord key => [(a, key, [key])] -> Maybe [a]
findCycle g = findCycleImplicit vs (labels M.!) (neighbors M.!) -- XXX M.!
 where
  vs = map (\(_, v, _) -> v) g
  labels = M.fromList (map (\(a, v, _) -> (v, a)) g)
  neighbors = M.fromList (map (\(_, v, nbrs) -> (v, nbrs)) g)

-- | A more generic version of 'findCycle' which takes as input an
--   implicit graph description: a list of vertices, a function
--   mapping vertices to labels, and a function mapping each vertex to
--   its (outgoing) neighbors.
findCycleImplicit :: forall v a. Ord v => [v] -> (v -> a) -> (v -> [v]) -> Maybe [a]
findCycleImplicit vertices label neighbors = flip evalState S.empty $ do
  (fmap . map) label <$> dfsL emptyDFSPath vertices
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

failOnCyclicGraph ::
  Ord key =>
  Text ->
  (a -> Text) ->
  [(a, key, [key])] ->
  Either Text ()
failOnCyclicGraph graphType keyFunction gEdges =
  forM_ (findCycle gEdges) $ \cyc ->
    Left $
      T.unwords
        [ graphType
        , "graph contains a cycle:"
        , brackets . T.intercalate " -> " . fmap keyFunction $ cyc
        ]
