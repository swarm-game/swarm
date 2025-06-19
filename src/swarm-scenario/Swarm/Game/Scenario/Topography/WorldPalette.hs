-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.WorldPalette where

import Control.Lens hiding (from, (.=), (<.>))
import Data.Aeson.KeyMap qualified as KM
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import Swarm.Game.Entity
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Palette
import Swarm.Game.Terrain (TerrainType)
import Swarm.Util.Erasable

-- | A world palette maps characters to 'Cell' values.
type WorldPalette e = StructurePalette (PCell e)

type TerrainWith a = (TerrainType, Erasable a)

cellToTerrainPair :: CellPaintDisplay -> TerrainWith EntityFacade
cellToTerrainPair (Cell terrain erasableEntity _) = (terrain, erasableEntity)

toCellPaintDisplay :: Cell -> CellPaintDisplay
toCellPaintDisplay (Cell terrain maybeEntity r) =
  Cell terrain (mkFacade <$> maybeEntity) r

toKey :: TerrainWith EntityFacade -> TerrainWith EntityName
toKey = fmap $ fmap (\(EntityFacade eName _display _hdg) -> eName)

-- | We want to identify all of the unique (terrain, entity facade) pairs.
-- However, "EntityFacade" includes a "Display" record, which contains more
-- fields than desirable for use as a unique key.
-- Therefore, we extract just the entity name for use in a
-- (terrain, entity name) key, and couple it with the original
-- (terrain, entity facade) pair in a Map.
getUniqueTerrainFacadePairs ::
  [CellPaintDisplay] ->
  M.Map (TerrainWith EntityName) (TerrainWith EntityFacade)
getUniqueTerrainFacadePairs cellGrid =
  M.fromList $ map genTuple cellGrid
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
  -- | Mask char
  Char ->
  Grid (Maybe CellPaintDisplay) ->
  String
constructWorldMap mappedPairs maskChar =
  unlines . getRows . fmap renderMapCell
 where
  invertedMappedPairs = map (swap . fmap toKey) mappedPairs

  renderMapCell :: Maybe CellPaintDisplay -> Char
  renderMapCell maybeC = case maybeC of
    Nothing -> maskChar
    Just c ->
      -- NOTE: This lookup should never fail; if it does for some
      -- reason, return Z rather than crash the game
      M.findWithDefault 'Z' k $
        M.fromList invertedMappedPairs
     where
      k = toKey $ cellToTerrainPair c

-- | All alphanumeric characters. These are used as supplemental
-- map placeholders in case a pre-existing display character is
-- not available to re-use.
genericCharacterPool :: Set.Set Char
genericCharacterPool = Set.fromList $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

data PaletteAndMaskChar = PaletteAndMaskChar
  { paletteEntries :: WorldPalette EntityFacade
  , reservedMaskChar :: Maybe Char
  -- ^ represents a transparent cell
  }

-- | Note that display characters are not unique
-- across different entities! However, the palette KeyMap
-- as a conveyance serves to dedupe them.
prepForJson ::
  PaletteAndMaskChar ->
  Grid (Maybe CellPaintDisplay) ->
  (String, KM.KeyMap CellPaintDisplay)
prepForJson (PaletteAndMaskChar (StructurePalette _ _ suggestedPalette) maybeMaskChar) cellGrid =
  (constructWorldMap mappedPairs maskCharacter cellGrid, constructPalette mappedPairs)
 where
  preassignments :: [(Char, TerrainWith EntityFacade)]
  preassignments =
    map (fmap (cellToTerrainPair . standardCell)) $
      M.toList suggestedPalette

  entityCells :: M.Map (TerrainWith EntityName) (TerrainWith EntityFacade)
  entityCells = getUniqueTerrainFacadePairs $ catMaybes $ allMembers cellGrid

  unassignedCells :: M.Map (TerrainWith EntityName) (TerrainWith EntityFacade)
  unassignedCells =
    M.withoutKeys entityCells $
      Set.fromList $
        map (toKey . snd) preassignments

  (maskCharacter, availableCharacterPool) = case maybeMaskChar of
    Just c -> (c, genericCharacterPool)
    Nothing -> Set.deleteFindMin genericCharacterPool

  unassignedCharacters :: Set.Set Char
  unassignedCharacters =
    -- TODO (#1149): How can we efficiently use the Unicode categories (in "Data.Char")
    -- to generate this pool?
    Set.difference availableCharacterPool usedCharacters
   where
    usedCharacters =
      Set.fromList $
        map fst preassignments

  newlyAssignedPairs :: [(Char, TerrainWith EntityFacade)]
  newlyAssignedPairs = zip (Set.toList unassignedCharacters) $ M.elems unassignedCells

  mappedPairs = preassignments <> newlyAssignedPairs
