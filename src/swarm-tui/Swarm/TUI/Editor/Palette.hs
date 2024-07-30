{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.Palette where

import Control.Lens
import Control.Monad (guard)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Swarm.Game.Display (Display, defaultChar)
import Swarm.Game.Entity (Entity, EntityName, entitiesByName)
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Navigation.Portal (Navigation (..))
import Swarm.Game.Scenario.Topography.ProtoCell
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Game.Terrain (TerrainMap, TerrainType, getTerrainDefaultPaletteChar, terrainByName)
import Swarm.Game.Universe
import Swarm.Language.Text.Markdown (fromText)
import Swarm.TUI.Editor.Json (SkeletonScenario (SkeletonScenario))
import Swarm.Util (binTuples, histogram)
import Swarm.Util.Erasable

makeSuggestedPalette ::
  TerrainMap ->
  Map Char (AugmentedCell Entity) ->
  Grid (Maybe CellPaintDisplay) ->
  Map Char (AugmentedCell EntityFacade)
makeSuggestedPalette tm originalScenarioPalette cellGrid =
  M.map (SignpostableCell Nothing Nothing)
    . M.fromList
    . M.elems
    -- NOTE: the left-most maps take precedence!
    $ paletteCellsByKey <> pairsWithDisplays <> terrainOnlyPalette
 where
  cellList = catMaybes $ allMembers cellGrid

  getMaybeEntityDisplay :: PCell EntityFacade -> Maybe (EntityName, Display)
  getMaybeEntityDisplay (Cell _terrain (erasableToMaybe -> maybeEntity) _) = do
    EntityFacade eName d <- maybeEntity
    return (eName, d)

  getMaybeEntityNameTerrainPair :: PCell EntityFacade -> Maybe (EntityName, TerrainType)
  getMaybeEntityNameTerrainPair (Cell terrain (erasableToMaybe -> maybeEntity) _) = do
    EntityFacade eName _ <- maybeEntity
    return (eName, terrain)

  getEntityTerrainMultiplicity :: Map EntityName (Map TerrainType Int)
  getEntityTerrainMultiplicity =
    M.map histogram $ binTuples $ mapMaybe getMaybeEntityNameTerrainPair cellList

  usedEntityDisplays :: Map EntityName Display
  usedEntityDisplays =
    M.fromList $ mapMaybe getMaybeEntityDisplay cellList

  -- Finds the most-used terrain type (the "mode" in the statistical sense)
  -- paired with each entity
  entitiesWithModalTerrain :: [(TerrainType, EntityName)]
  entitiesWithModalTerrain =
    map (swap . fmap (fst . NE.head))
      . mapMaybe sequenceA
      . M.toList
      $ M.map (NE.nonEmpty . sortOn snd . M.toList) getEntityTerrainMultiplicity

  invertPaletteMapToDedupe ::
    Map a CellPaintDisplay ->
    [(TerrainWith EntityName, (a, CellPaintDisplay))]
  invertPaletteMapToDedupe =
    map (\x@(_, c) -> (toKey $ cellToTerrainPair c, x)) . M.toList

  paletteCellsByKey :: Map (TerrainWith EntityName) (Char, CellPaintDisplay)
  paletteCellsByKey =
    M.map (NE.head . NE.sortWith toSortVal)
      . binTuples
      . invertPaletteMapToDedupe
      $ originalPalette
   where
    toSortVal (symbol, Cell _terrain _maybeEntity robots) = Down (null robots, symbol)

  excludedPaletteChars :: Set Char
  excludedPaletteChars = Set.fromList [' ']

  originalPalette :: Map Char CellPaintDisplay
  originalPalette =
    M.map (toCellPaintDisplay . standardCell) originalScenarioPalette

  pairsWithDisplays :: Map (TerrainWith EntityName) (Char, CellPaintDisplay)
  pairsWithDisplays = M.fromList $ mapMaybe g entitiesWithModalTerrain
   where
    g (terrain, eName) = do
      eDisplay <- M.lookup eName usedEntityDisplays
      let displayChar = eDisplay ^. defaultChar
      guard $ Set.notMember displayChar excludedPaletteChars
      let cell = Cell terrain (EJust $ EntityFacade eName eDisplay) []
      return ((terrain, EJust eName), (displayChar, cell))

  -- TODO (#1153): Filter out terrain-only palette entries that aren't actually
  -- used in the map.
  terrainOnlyPalette :: Map (TerrainWith EntityName) (Char, CellPaintDisplay)
  terrainOnlyPalette = M.fromList . map f . M.keys $ terrainByName tm
   where
    f x = ((x, ENothing), (getTerrainDefaultPaletteChar x, Cell x ENothing []))

-- | Generate a \"skeleton\" scenario with placeholders for certain required fields
constructScenario :: Maybe Scenario -> Grid (Maybe CellPaintDisplay) -> SkeletonScenario
constructScenario maybeOriginalScenario cellGrid =
  SkeletonScenario
    (maybe 1 (^. scenarioMetadata . scenarioVersion) maybeOriginalScenario)
    (maybe "My Scenario" (^. scenarioMetadata . scenarioName) maybeOriginalScenario)
    (maybe (fromText "The scenario description...") (^. scenarioOperation . scenarioDescription) maybeOriginalScenario)
    -- (maybe True (^. scenarioCreative) maybeOriginalScenario)
    True
    (M.elems $ entitiesByName customEntities)
    wd
    [] -- robots
 where
  tem = maybe mempty (^. scenarioLandscape . scenarioTerrainAndEntities) maybeOriginalScenario
  customEntities = tem ^. entityMap
  wd =
    WorldDescription
      { offsetOrigin = False
      , scrollable = True
      , palette = StructurePalette suggestedPalette
      , area = PositionedGrid upperLeftCoord cellGrid
      , navigation = Navigation mempty mempty
      , placedStructures = mempty
      , worldName = DefaultRootSubworld
      , worldProg = Nothing
      }

  extractPalette = unPalette . palette . NE.head . (^. scenarioLandscape . scenarioWorlds)
  originalPalette = maybe mempty extractPalette maybeOriginalScenario
  suggestedPalette = makeSuggestedPalette (tem ^. terrainMap) originalPalette cellGrid

  upperLeftCoord =
    Location
      (negate $ w `div` 2)
      (h `div` 2)
   where
    AreaDimensions w h = getGridDimensions cellGrid
