-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Precomputation for structure recognizer.
--
-- = Search process overview
--
-- 2D structures may be defined at the
-- <https://github.com/swarm-game/swarm/blob/main/data/scenarios/_doc-fragments/SCHEMA.md#top-level toplevel of a scenario file>.
-- Upon scenario load, all of the predefined structures that are marked
-- as @"recognize"@ are compiled into searcher state machines.
--
-- When an entity is placed on any cell in the world, the
-- 'Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking.entityModified'
-- function is called, which looks up a customized searcher based
-- on the type of placed entity.
--
-- The first searching stage looks for any member row of all participating
-- structure definitions that contains the placed entity.
-- The value returned by the searcher is a second-stage searcher state machine,
-- which this time searches for complete structures of which the found row may
-- be a member.
--
-- Both the first stage and second stage searcher know to start the search
-- at a certain offset horizontally or vertically from the placed entity,
-- based on where within a structure that entity (or row) may occur.
--
-- Upon locating a complete structure, it is added to a registry
-- (see 'Swarm.Game.Scenario.Topography.Structure.Recognition.Registry.FoundRegistry'), which
-- supports lookups by either name or by location (using two different
-- maps maintained in parallel). The map by location is used to remove
-- a structure from the registry if a member entity is changed.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (
  -- * Main external interface
  mkAutomatons,

  -- * Helper functions
  populateStaticFoundStructures,
  getEntityGrid,
  lookupStaticPlacements,
) where

import Control.Arrow ((&&&))
import Control.Lens (view)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Prep
import Data.Set qualified as Set
import Swarm.Game.Entity (Entity, EntityName, entityName)
import Swarm.Game.Scenario (StaticStructureInfo (..))
import Swarm.Game.Scenario.Topography.Cell (PCell, cellEntity)
import Swarm.Game.Scenario.Topography.Grid (Grid, getRows)
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransform)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic (..))
import Swarm.Language.Syntax.Direction (AbsoluteDir)
import Swarm.Util (histogram)
import Swarm.Util.Erasable (erasableToMaybe)

getEntityGrid :: Grid (Maybe (PCell Entity)) -> [SymbolSequence Entity]
getEntityGrid = map (map ((erasableToMaybe . cellEntity) =<<)) . getRows

-- | Create Aho-Corasick matchers that will recognize all of the
-- provided structure definitions
mkAutomatons ::
  [SymmetryAnnotatedGrid (Maybe (PCell Entity))] ->
  RecognizerAutomatons (PCell Entity) EntityName Entity
mkAutomatons xs =
  RecognizerAutomatons
    infos
    (mkEntityLookup (view entityName) rotatedGrids)
 where
  rotatedGrids = concatMap (extractGrids . namedGrid) xs

  process g = StructureInfo g (getEntityGrid $ structure $ namedGrid g) . histogram . concatMap catMaybes . getEntityGrid . structure $ namedGrid g
  infos = M.fromList $ map (name . namedGrid &&& process) xs

extractOrientedGrid ::
  NamedGrid (Maybe (PCell Entity)) ->
  AbsoluteDir ->
  StructureWithGrid (PCell Entity) Entity
extractOrientedGrid x d = StructureWithGrid x d $ getEntityGrid g
 where
  g = applyOrientationTransform (Orientation d False) $ structure x

-- | At this point, we have already ensured that orientations
-- redundant by rotational symmetry have been excluded
-- (i.e. at Scenario validation time).
extractGrids :: NamedGrid (Maybe (PCell Entity)) -> [StructureWithGrid (PCell Entity) Entity]
extractGrids x = map (extractOrientedGrid x) $ Set.toList $ recognize x

-- | The output list of 'FoundStructure' records is not yet
-- vetted; the 'ensureStructureIntact' function will subsequently
-- filter this list.
lookupStaticPlacements :: StaticStructureInfo -> [FoundStructure (PCell Entity) Entity]
lookupStaticPlacements (StaticStructureInfo structDefs thePlacements) =
  concatMap f $ M.toList thePlacements
 where
  definitionMap = M.fromList $ map ((name &&& id) . namedGrid) structDefs

  f (subworldName, locatedList) = mapMaybe g locatedList
   where
    g (LocatedStructure theName d loc) = do
      sGrid <- M.lookup theName definitionMap
      return $ FoundStructure (extractOrientedGrid sGrid d) $ Cosmic subworldName loc
