-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Graph-based heuristics for arranging the
-- order of sections in the schema docs
module Swarm.Doc.Schema.Arrangement (sortAndPruneSchemas) where

import Data.Graph
import Data.Set qualified as Set
import Swarm.Doc.Schema.Parse
import Swarm.Doc.Schema.Refined
import Swarm.Doc.Schema.SchemaType

-- | Sort the schemas in topological order.
--
-- Only includes schema files that are reachable from
-- the root schema
-- (i.e. exclude @entities.json@ and @recipes.json@,
-- which are used independently to validate @entities.yaml@
-- and @recipes.yaml@).
sortAndPruneSchemas ::
  SchemaIdReference ->
  [SchemaData] ->
  [SchemaData]
sortAndPruneSchemas rootSchemaKey schemas =
  reverse . flattenSCCs . stronglyConnComp $ reachableEdges
 where
  rawEdgeList = map getNodeEdgesEntry schemas
  (graph, _nodeFromVertex, vertexFromKey) = graphFromEdges rawEdgeList
  reachableVertices = Set.fromList $ maybe [] (reachable graph) $ vertexFromKey rootSchemaKey

  reachableEdges = filter f rawEdgeList
  f (_, k, _) = maybe False (`Set.member` reachableVertices) . vertexFromKey $ k

getNodeEdgesEntry ::
  SchemaData ->
  (SchemaData, SchemaIdReference, [SchemaIdReference])
getNodeEdgesEntry sd@(SchemaData fp schem _) =
  ( sd
  , fromFilePath fp
  , Set.toList $ extractReferences $ content schem
  )
