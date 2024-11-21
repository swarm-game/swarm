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
-- If we observe a row in the world that happens to occur in a structure, we use both
-- the horizontal found offset and the index of the row within this structure to compute
-- the expected world location of the candidate structure.
-- Then we perform a full scan of that candidate structure against the world to verify
-- the match.
--
-- Upon locating a complete structure, it is added to a registry
-- (see 'Swarm.Game.Scenario.Topography.Structure.Recognition.Registry.FoundRegistry'), which
-- supports lookups by either name or by location (using two different
-- maps maintained in parallel). The map by location is used to remove
-- a structure from the registry if a member entity is changed.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (
  -- * Main external interface
  mkAutomatons,

  -- * Types
  GenericEntLocator,

  -- * Helper functions
  populateStaticFoundStructures,
  getEntityGrid,
  lookupStaticPlacements,
  ensureStructureIntact,
) where

import Control.Arrow ((&&&))
import Control.Monad (forM_, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT)
import Data.Either.Combinators (leftToMaybe)
import Data.Hashable (Hashable)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Swarm.Game.Location (Location, asVector)
import Swarm.Game.Scenario.Topography.Area (getGridDimensions, rectWidth)
import Swarm.Game.Scenario.Topography.Grid (getRows, mapIndexedMembers, mkGrid)
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransform, getStructureName)
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Recognition.Prep (
  mkEntityLookup,
 )
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (
  FoundRegistry,
  foundByLocation,
  populateStaticFoundStructures,
 )
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic (..), offsetBy)
import Swarm.Game.World.Coords (coordsToLoc)
import Swarm.Language.Syntax.Direction (AbsoluteDir)
import Swarm.Util (histogram)

-- | Interface that provides monadic access to
-- querying entities at locations.
-- The provider may be a 'State' monad or just
-- a 'Reader'.
--
-- 's' is the state variable, 'a' is the return type.
type GenericEntLocator s a = Cosmic Location -> s (Maybe a)

getEntityGrid :: (Maybe b -> Maybe a) -> NamedGrid (Maybe b) -> [[Maybe a]]
getEntityGrid extractor = getRows . fmap extractor . structure

-- | Create Aho-Corasick matchers that will recognize all of the
-- provided structure definitions
mkAutomatons ::
  (Ord a, Hashable a) =>
  (Maybe b -> Maybe a) ->
  [SymmetryAnnotatedGrid (Maybe b)] ->
  RecognizerAutomatons (Maybe b) a
mkAutomatons extractor xs =
  RecognizerAutomatons
    infos
    (mkEntityLookup rotatedGrids)
 where
  rotatedGrids = concatMap (extractGrids extractor . namedGrid) xs

  process g = StructureInfo g entGrid countsMap
   where
    entGrid = getEntityGrid extractor $ namedGrid g
    countsMap = histogram $ concatMap catMaybes entGrid

  infos =
    M.fromList $
      map (getStructureName . name . namedGrid &&& process) xs

extractOrientedGrid ::
  (Maybe b -> Maybe a) ->
  NamedGrid (Maybe b) ->
  AbsoluteDir ->
  StructureWithGrid (Maybe b) a
extractOrientedGrid extractor x d =
  StructureWithGrid wrapped d w $ getEntityGrid extractor g
 where
  w = RowWidth . rectWidth . getGridDimensions $ structure g
  wrapped = NamedOriginal (getStructureName $ name x) x
  g = applyOrientationTransform (Orientation d False) <$> x

-- | At this point, we have already ensured that orientations
-- redundant by rotational symmetry have been excluded
-- (i.e. at Scenario validation time).
extractGrids ::
  (Maybe b -> Maybe a) ->
  NamedGrid (Maybe b) ->
  [StructureWithGrid (Maybe b) a]
extractGrids extractor x =
  map (extractOrientedGrid extractor x) $ Set.toList $ recognize x

-- | The output list of 'FoundStructure' records is not yet
-- vetted; the 'ensureStructureIntact' function will subsequently
-- filter this list.
lookupStaticPlacements ::
  (Maybe b -> Maybe a) ->
  StaticStructureInfo b ->
  [FoundStructure (Maybe b) a]
lookupStaticPlacements extractor (StaticStructureInfo structDefs thePlacements) =
  concatMap f $ M.toList thePlacements
 where
  definitionMap = M.fromList $ map ((name &&& id) . namedGrid) structDefs

  f (subworldName, locatedList) = mapMaybe g locatedList
   where
    g (LocatedStructure theName d loc) = do
      sGrid <- M.lookup theName definitionMap
      return $ FoundStructure (extractOrientedGrid extractor sGrid d) $ Cosmic subworldName loc

-- | Matches definitions against the placements.
-- Fails fast (short-circuits) if a non-matching
-- cell is encountered.
--
-- Returns 'Nothing' if there is no discrepancy between the match subject and world content.
-- Returns the first observed mismatch cell otherwise.
ensureStructureIntact ::
  (Monad s, Hashable a) =>
  FoundRegistry b a ->
  GenericEntLocator s a ->
  FoundStructure b a ->
  s (Maybe (StructureIntactnessFailure a))
ensureStructureIntact registry entLoader (FoundStructure (StructureWithGrid _ _ (RowWidth w) grid) upperLeft) = do
  fmap leftToMaybe . runExceptT . mapM checkLoc $ zip [0 ..] allLocPairs
 where
  checkLoc (idx, (maybeTemplateEntity, loc)) =
    forM_ maybeTemplateEntity $ \x -> do
      e <- lift $ entLoader loc

      forM_ (M.lookup loc $ foundByLocation registry) $ \s ->
        except
          . Left
          . StructureIntactnessFailure (AlreadyUsedBy $ distillLabel $ structureWithGrid s) idx
          $ fromIntegral w * length grid

      unless (e == Just x)
        . except
        . Left
        . StructureIntactnessFailure (DiscrepantEntity $ EntityDiscrepancy x e) idx
        $ fromIntegral w * length grid

  f = fmap ((upperLeft `offsetBy`) . asVector . coordsToLoc) . swap
  allLocPairs = mapIndexedMembers (curry f) $ mkGrid grid
