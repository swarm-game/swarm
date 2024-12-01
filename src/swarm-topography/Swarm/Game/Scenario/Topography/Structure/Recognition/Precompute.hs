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
  lookupStaticPlacements,
  ensureStructureIntact,
) where

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import Control.Monad (forM_, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT)
import Data.Either.Combinators (leftToMaybe)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import Swarm.Game.Location (Location, asVector)
import Swarm.Game.Scenario.Topography.Area (getNEGridDimensions, rectWidth)
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransformNE)
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
import Swarm.Game.Scenario.Topography.Structure.Recognition.Symmetry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic (..), offsetBy, planar)
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

-- | Create Aho-Corasick matchers that will recognize all of the
-- provided structure definitions
mkAutomatons ::
  (Ord a, Hashable a, Eq b) =>
  (Maybe b -> Maybe a) ->
  [NamedArea (NonEmptyGrid (Maybe b))] ->
  Either
    T.Text
    ( [SymmetryAnnotatedGrid (ExtractedArea b a)]
    , RecognizerAutomatons b a
    )
mkAutomatons extractor rawGrids = do
  onlyNonempties <- mapM checkSymmetry extractedItems
  let rotatedGrids = concatMap (extractGrids . namedGrid) onlyNonempties
      infos =
        M.fromList $
          map ((name . namedGrid . annotatedGrid &&& id) . process) onlyNonempties
  return
    ( onlyNonempties
    , RecognizerAutomatons
        infos
        (mkEntityLookup rotatedGrids)
    )
 where
  extractedItems = map (\g -> ExtractedArea g $ extractor <$> structure g) rawGrids

  process g = StructureInfo (fmap originalItem g) entGrid countsMap
   where
    entGrid = extractedGrid $ namedGrid g
    countsMap = histogram . catMaybes . NE.toList $ allMembersNE entGrid

extractOrientedGrid ::
  ExtractedArea b a ->
  AbsoluteDir ->
  StructureWithGrid (NonEmptyGrid (Maybe b)) a
extractOrientedGrid (ExtractedArea x neGrid) d =
  StructureWithGrid wrapped d w $
    applyOrientationTransformNE (Orientation d False) neGrid
 where
  w = RowWidth . rectWidth . getNEGridDimensions $ neGrid
  wrapped = NamedOriginal (name x) x

-- |
-- At this point, we have already ensured that orientations
-- redundant by rotational symmetry have been excluded
-- (i.e. at Scenario validation time).
extractGrids ::
  ExtractedArea b a ->
  [StructureWithGrid (NonEmptyGrid (Maybe b)) a]
extractGrids x =
  map (extractOrientedGrid x) orientations
 where
  orientations = Set.toList $ recognize $ originalItem x

-- | The output list of 'FoundStructure' records is not yet
-- vetted; the 'ensureStructureIntact' function will subsequently
-- filter this list.
lookupStaticPlacements ::
  StaticStructureInfo b a ->
  [FoundStructure (NonEmptyGrid (Maybe b)) a]
lookupStaticPlacements (StaticStructureInfo structDefs _ thePlacements) =
  concatMap f $ M.toList thePlacements
 where
  definitionMap = M.fromList $ map ((name . originalItem &&& id) . namedGrid) structDefs

  f (subworldName, locatedList) = mapMaybe g locatedList
   where
    g (LocatedStructure (OrientedStructure theName d) loc) = do
      sGrid <- M.lookup theName definitionMap
      return $
        PositionedStructure (Cosmic subworldName loc) $
          extractOrientedGrid sGrid d

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
ensureStructureIntact registry entLoader (PositionedStructure upperLeft (StructureWithGrid _ _ _ grid)) = do
  fmap leftToMaybe . runExceptT $ mapM checkLoc allLocPairs
 where
  gridArea = getNEGridDimensions grid
  checkLoc (maybeTemplateEntity, loc) =
    forM_ maybeTemplateEntity $ \x -> do
      e <- lift $ entLoader loc

      forM_ (M.lookup loc $ foundByLocation registry) $ \s ->
        errorPrefix
          . AlreadyUsedBy
          . distillLabel
          $ structureWithGrid s

      unless (e == Just x)
        . errorPrefix
        $ DiscrepantEntity
        $ EntityDiscrepancy x e
   where
    errorPrefix =
      except
        . Left
        . StructureIntactnessFailure (loc ^. planar) gridArea

  f = fmap ((upperLeft `offsetBy`) . asVector . coordsToLoc) . swap
  allLocPairs = mapWithCoordsNE (curry f) grid
