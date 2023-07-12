{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.WorldPalette where

import Control.Arrow (first)
import Control.Lens hiding (from, (.=), (<.>))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Swarm.Game.Entity
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Terrain (TerrainType)
import Swarm.Util.Yaml

-- | A world palette maps characters to 'Cell' values.
newtype WorldPalette e = WorldPalette
  {unPalette :: KeyMap (AugmentedCell e)}
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) (WorldPalette Entity) where
  parseJSONE = withObjectE "palette" $ fmap WorldPalette . mapM parseJSONE

type TerrainWith a = (TerrainType, Maybe a)

cellToTerrainPair :: CellPaintDisplay -> TerrainWith EntityFacade
cellToTerrainPair (Cell terrain maybeEntity _) = (terrain, maybeEntity)

toCellPaintDisplay :: Cell -> CellPaintDisplay
toCellPaintDisplay (Cell terrain maybeEntity r) =
  Cell terrain (mkFacade <$> maybeEntity) r

toKey :: TerrainWith EntityFacade -> TerrainWith EntityName
toKey = fmap $ fmap (\(EntityFacade eName _display) -> eName)

-- | We want to identify all of the unique (terrain, entity facade) pairs.
-- However, "EntityFacade" includes a "Display" record, which contains more
-- fields than desirable for use as a unique key.
-- Therefore, we extract just the entity name for use in a
-- (terrain, entity name) key, and couple it with the original
-- (terrain, entity facade) pair in a Map.
getUniqueTerrainFacadePairs ::
  [[CellPaintDisplay]] ->
  M.Map (TerrainWith EntityName) (TerrainWith EntityFacade)
getUniqueTerrainFacadePairs cellGrid =
  M.fromList $ concatMap (map genTuple) cellGrid
 where
  genTuple c =
    (toKey terrainEfd, terrainEfd)
   where
    terrainEfd = cellToTerrainPair c

constructPalette ::
  [(Char, TerrainWith EntityFacade)] ->
  KM.KeyMap CellPaintDisplay
constructPalette mappedPairs =
  KM.fromMapText terrainEntityPalette
 where
  g (terrain, maybeEfd) = Cell terrain maybeEfd []
  terrainEntityPalette = M.fromList $ map (bimap T.singleton g) mappedPairs

constructWorldMap ::
  [(Char, TerrainWith EntityFacade)] ->
  [[CellPaintDisplay]] ->
  Text
constructWorldMap mappedPairs =
  T.unlines . map (T.pack . map renderMapCell)
 where
  invertedMappedPairs = map (swap . fmap toKey) mappedPairs

  renderMapCell c =
    -- NOTE: This lookup should never fail
    M.findWithDefault (error "Palette lookup failed!") k $
      M.fromList invertedMappedPairs
   where
    k = toKey $ cellToTerrainPair c

-- | All alphanumeric characters. These are used as supplemental
-- map placeholders in case a pre-existing display character is
-- not available to re-use.
genericCharacterPool :: Set.Set Char
genericCharacterPool = Set.fromList $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

-- | Note that display characters are not unique
-- across different entities! However, the palette KeyMap
-- as a conveyance serves to dedupe them.
prepForJson ::
  WorldPalette EntityFacade ->
  [[CellPaintDisplay]] ->
  (Text, KM.KeyMap CellPaintDisplay)
prepForJson (WorldPalette suggestedPalette) cellGrid =
  (constructWorldMap mappedPairs cellGrid, constructPalette mappedPairs)
 where
  preassignments :: [(Char, TerrainWith EntityFacade)]
  preassignments =
    map (first T.head . fmap (cellToTerrainPair . standardCell)) $
      M.toList $
        KM.toMapText suggestedPalette

  entityCells :: M.Map (TerrainWith EntityName) (TerrainWith EntityFacade)
  entityCells = getUniqueTerrainFacadePairs cellGrid

  unassignedCells :: M.Map (TerrainWith EntityName) (TerrainWith EntityFacade)
  unassignedCells =
    M.withoutKeys entityCells $
      Set.fromList $
        map (toKey . snd) preassignments

  unassignedCharacters :: Set.Set Char
  unassignedCharacters =
    -- TODO (#1149): How can we efficiently use the Unicode categories (in "Data.Char")
    -- to generate this pool?
    Set.difference genericCharacterPool $
      Set.fromList $
        map fst preassignments

  newlyAssignedPairs :: [(Char, TerrainWith EntityFacade)]
  newlyAssignedPairs = zip (Set.toList unassignedCharacters) $ M.elems unassignedCells

  mappedPairs = preassignments <> newlyAssignedPairs
