{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.Palette where

import Control.Lens
import Control.Monad (guard)
import Data.Aeson.KeyMap qualified as KM
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import Swarm.Game.Display (Display, defaultChar)
import Swarm.Game.Entity (entitiesByName, EntityName)
import Swarm.Game.Location
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..), getAreaDimensions)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Navigation.Portal (Navigation (..))
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Game.Terrain (TerrainType, getTerrainDefaultPaletteChar)
import Swarm.Game.Universe
import Swarm.TUI.Editor.Json (SkeletonScenario (SkeletonScenario))
import Swarm.Util (binTuples, histogram)
import Swarm.Util qualified as U
import Swarm.Util.Erasable

makeSuggestedPalette :: Maybe Scenario -> [[CellPaintDisplay]] -> KM.KeyMap (AugmentedCell EntityFacade)
makeSuggestedPalette maybeOriginalScenario cellGrid =
  KM.fromMapText
    . M.map (AugmentedCell Nothing)
    . M.fromList
    . M.elems
    -- NOTE: the left-most maps take precedence!
    $ paletteCellsByKey <> pairsWithDisplays <> terrainOnlyPalette
 where
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
    M.map histogram $ binTuples $ concatMap (mapMaybe getMaybeEntityNameTerrainPair) cellGrid

  usedEntityDisplays :: Map EntityName Display
  usedEntityDisplays =
    M.fromList $ concatMap (mapMaybe getMaybeEntityDisplay) cellGrid

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

  paletteCellsByKey :: Map (TerrainWith EntityName) (T.Text, CellPaintDisplay)
  paletteCellsByKey =
    M.map (NE.head . NE.sortWith toSortVal)
      . binTuples
      . invertPaletteMapToDedupe
      $ KM.toMapText originalPalette
   where
    toSortVal (symbol, Cell _terrain _maybeEntity robots) = Down (null robots, symbol)

  excludedPaletteChars :: Set Char
  excludedPaletteChars = Set.fromList [' ']

  originalPalette :: KM.KeyMap CellPaintDisplay
  originalPalette =
    KM.map (toCellPaintDisplay . standardCell) $
      maybe mempty (unPalette . palette . NE.head . (^. scenarioWorlds)) maybeOriginalScenario

  pairsWithDisplays :: Map (TerrainWith EntityName) (T.Text, CellPaintDisplay)
  pairsWithDisplays = M.fromList $ mapMaybe g entitiesWithModalTerrain
   where
    g (terrain, eName) = do
      eDisplay <- M.lookup eName usedEntityDisplays
      let displayChar = eDisplay ^. defaultChar
      guard $ Set.notMember displayChar excludedPaletteChars
      let cell = Cell terrain (EJust $ EntityFacade eName eDisplay) []
      return ((terrain, EJust eName), (T.singleton displayChar, cell))

  -- TODO (#1153): Filter out terrain-only palette entries that aren't actually
  -- used in the map.
  terrainOnlyPalette :: Map (TerrainWith EntityName) (T.Text, CellPaintDisplay)
  terrainOnlyPalette = M.fromList $ map f U.listEnums
   where
    f x = ((x, ENothing), (T.singleton $ getTerrainDefaultPaletteChar x, Cell x ENothing []))

-- | Generate a \"skeleton\" scenario with placeholders for certain required fields
constructScenario :: Maybe Scenario -> [[CellPaintDisplay]] -> SkeletonScenario
constructScenario maybeOriginalScenario cellGrid =
  SkeletonScenario
    (maybe 1 (^. scenarioVersion) maybeOriginalScenario)
    (maybe "My Scenario" (^. scenarioName) maybeOriginalScenario)
    (maybe "The scenario description..." (^. scenarioDescription) maybeOriginalScenario)
    -- (maybe True (^. scenarioCreative) maybeOriginalScenario)
    True
    (M.elems $ entitiesByName customEntities)
    wd
    [] -- robots
 where
  customEntities = maybe mempty (^. scenarioEntities) maybeOriginalScenario
  wd =
    WorldDescription
      { offsetOrigin = False
      , scrollable = True
      , palette = WorldPalette suggestedPalette
      , ul = upperLeftCoord
      , area = cellGrid
      , navigation = Navigation mempty mempty
      , worldName = DefaultRootSubworld
      , worldProg = Nothing
      }

  suggestedPalette = makeSuggestedPalette maybeOriginalScenario cellGrid

  upperLeftCoord =
    Location
      (negate $ w `div` 2)
      (h `div` 2)
   where
    AreaDimensions w h = getAreaDimensions cellGrid
