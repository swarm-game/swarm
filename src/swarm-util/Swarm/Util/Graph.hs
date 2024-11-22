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
import Data.Array ((!))
import Data.Array.ST
import Data.Graph (SCC (..), Vertex, graphFromEdges)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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
data DFSPath = DFSPath IntSet [Vertex]

emptyDFSPath :: DFSPath
emptyDFSPath = DFSPath IS.empty []

appendPath :: DFSPath -> Vertex -> DFSPath
appendPath (DFSPath s p) v = DFSPath (IS.insert v s) (v : p)

-- | Find a cycle in a directed graph (if any exist) via DFS.
--
-- >>> findCycle [("a", 0, [0])]
-- Just ["a"]
-- >>> findCycle [("a", 0, [1]), ("b", 1, [])]
-- Nothing
-- >>> findCycle [("a", 0, [1]), ("b", 1, [0])]
-- Just ["a","b"]
-- >>> findCycle [("a", 0, [1]), ("b", 1, [2]), ("c", 2, [1])]
-- Just ["b","c"]
-- >>> findCycle [("a",3,[1]), ("b",1,[0,3]), ("c",2,[1]), ("d",0,[])]
-- Just ["b","a"]
-- >>> findCycle [("a",3,[]), ("b",1,[0,3]), ("c",2,[1]), ("d",0,[])]
-- Nothing
-- >>> findCycle [("a",3,[1]), ("b",1,[0,3]), ("c",2,[1]), ("d",0,[2])]
-- Just ["d","c","b"]
findCycle :: Ord key => [(a, key, [key])] -> Maybe [a]
findCycle es = runST $ do
  visited <- newArray (0, n - 1) False
  (fmap . map) (fst3 . v2l) <$> dfsL visited emptyDFSPath [0 .. n - 1]
 where
  n = length es
  (g, v2l, _) = graphFromEdges es
  fst3 (a, _, _) = a

  dfsL :: STUArray s Vertex Bool -> DFSPath -> [Vertex] -> ST s (Maybe [Vertex])
  dfsL _ _ [] = pure Nothing
  dfsL visited path (v : vs) = do
    found <- dfs visited path v
    case found of
      Nothing -> dfsL visited path vs
      Just cyc -> pure (Just cyc)

  dfs :: STUArray s Vertex Bool -> DFSPath -> Vertex -> ST s (Maybe [Vertex])
  dfs visited p@(DFSPath pathMembers path) v
    | v `IS.member` pathMembers = pure . Just . (v :) . reverse $ takeWhile (/= v) path
    | otherwise = do
        vis <- readArray visited v
        case vis of
          True -> pure Nothing
          False -> dfsL visited (appendPath p v) (g ! v)

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
