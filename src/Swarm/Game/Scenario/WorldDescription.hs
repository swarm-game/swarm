{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Scenario.WorldDescription where

import Control.Arrow (first)
import Control.Lens hiding (from, (.=), (<.>))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Terrain (TerrainType)
import Swarm.Util.Location
import Swarm.Util.Yaml
import Witch (into)

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A world palette maps characters to 'Cell' values.
newtype WorldPalette e = WorldPalette
  {unPalette :: KeyMap (PCell e)}
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) (WorldPalette Entity) where
  parseJSONE = withObjectE "palette" $ fmap WorldPalette . mapM parseJSONE

-- | A description of a world parsed from a YAML file.
-- This type is parameterized to accommodate Cells that
-- utilize a less stateful Entity type.
data PWorldDescription e = WorldDescription
  { defaultTerrain :: Maybe (PCell e)
  , offsetOrigin :: Bool
  , palette :: WorldPalette e
  , ul :: Location
  , area :: [[PCell e]]
  }
  deriving (Eq, Show)

type WorldDescription = PWorldDescription Entity

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> pure pal
      <*> liftE (v .:? "upperleft" .!= origin)
      <*> liftE ((v .:? "map" .!= "") >>= paintMap pal)

-- | "Paint" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'Cell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap :: MonadFail m => WorldPalette e -> Text -> m [[PCell e]]
paintMap pal = traverse (traverse toCell . into @String) . T.lines
 where
  toCell c = case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
    Nothing -> fail $ "Char not in world palette: " ++ show c
    Just cell -> return cell

------------------------------------------------------------
-- World editor
------------------------------------------------------------

type TerrainEntityNamePair = (TerrainType, Maybe EntityName)

type TerrainEntityFacadePair = (TerrainType, Maybe EntityFacade)

cellToTerrainEntityNamePair :: CellPaintDisplay -> TerrainEntityFacadePair
cellToTerrainEntityNamePair (Cell terrain maybeEntity _) = (terrain, maybeEntity)

toCellPaintDisplay :: Cell -> CellPaintDisplay
toCellPaintDisplay (Cell terrain maybeEntity r) =
  Cell terrain (mkPaint <$> maybeEntity) r

type WorldDescriptionPaint = PWorldDescription EntityFacade

instance ToJSON WorldDescriptionPaint where
  toJSON w =
    object
      [ "default" .= defaultTerrain w
      , "offset" .= offsetOrigin w
      , "palette" .= Y.toJSON paletteKeymap
      , "upperleft" .= ul w
      , "map" .= Y.toJSON mapText
      ]
   where
    cellGrid = area w
    suggestedPalette = palette w
    (mapText, paletteKeymap) = prepForJson suggestedPalette cellGrid

toKey :: TerrainEntityFacadePair -> TerrainEntityNamePair
toKey = fmap $ fmap (\(EntityFacade eName _display) -> eName)

getUniquePairs :: [[CellPaintDisplay]] -> M.Map TerrainEntityNamePair TerrainEntityFacadePair
getUniquePairs cellGrid =
  M.fromList $ concatMap (map genTuple) cellGrid
 where
  genTuple c =
    (toKey terrainEfd, terrainEfd)
   where
    terrainEfd = cellToTerrainEntityNamePair c

constructPalette ::
  [(Char, TerrainEntityFacadePair)] ->
  KM.KeyMap CellPaintDisplay
constructPalette mappedPairs =
  KM.fromMapText terrainEntityPalette
 where
  g (terrain, maybeEfd) = Cell terrain maybeEfd []
  terrainEntityPalette = M.fromList $ map (bimap T.singleton g) mappedPairs

constructWorldMap ::
  [(Char, TerrainEntityFacadePair)] ->
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
    k = toKey $ cellToTerrainEntityNamePair c

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
  preassignments :: [(Char, TerrainEntityFacadePair)]
  preassignments =
    map (first T.head . fmap cellToTerrainEntityNamePair) $
      M.toList $
        KM.toMapText suggestedPalette

  entityCells :: M.Map TerrainEntityNamePair TerrainEntityFacadePair
  entityCells = getUniquePairs cellGrid

  unassignedCells :: M.Map TerrainEntityNamePair TerrainEntityFacadePair
  unassignedCells =
    M.withoutKeys entityCells $
      Set.fromList $
        map (toKey . snd) preassignments

  unassignedCharacters :: Set.Set Char
  unassignedCharacters =
    -- TODO: How can we efficiently use the Unicode categories (in "Data.Char")
    -- to generate this pool?
    Set.difference genericCharacterPool $
      Set.fromList $
        map fst preassignments

  newlyAssignedPairs :: [(Char, TerrainEntityFacadePair)]
  newlyAssignedPairs = zip (Set.toList unassignedCharacters) $ M.elems unassignedCells

  mappedPairs = preassignments <> newlyAssignedPairs
