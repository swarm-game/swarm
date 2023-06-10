{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Structure where

import Control.Arrow ((&&&))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.WorldPalette
import Swarm.Language.Syntax (AbsoluteDir (DNorth))
import Swarm.Util.Yaml
import Witch (into)

newtype StructureName = StructureName Text
  deriving (Eq, Ord, Show, Generic, FromJSON)

data NamedStructure e = NamedStructure
  { name :: StructureName
  , structure :: PStructure e
  }
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) (NamedStructure Entity) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    sName <- liftE $ v .: "name"
    NamedStructure sName
      <$> v
      ..: "structure"

data PStructure e = Structure
  { area :: [[PCell e]]
  , structures :: [NamedStructure e]
  -- ^ structure definitions from parents shall be accessible by children
  , placements :: [Placement]
  }
  deriving (Eq, Show)

newtype MergedStructure e = MergedStructure [[PCell e]]

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
overlaySingleStructure ::
  M.Map StructureName (PStructure e) ->
  MergedStructure e ->
  (Placement, PStructure e) ->
  MergedStructure e
overlaySingleStructure
  inheritedStrucDefs
  (MergedStructure inputArea)
  (Placement _ (Location colOffset rowOffset) _orientation, struc) =
    MergedStructure $ zipWithPad mergeSingleRow inputArea paddedOverlayRows
   where
    zipWithPad f a b = zipWith f a $ b <> repeat Nothing
    MergedStructure overlayArea = mergeStructures inheritedStrucDefs struc

    mergeSingleRow inputRow maybeOverlayRow =
      zipWithPad fromMaybe inputRow paddedSingleOverlayRow
     where
      paddedSingleOverlayRow = maybe [] (sandwich $ negate colOffset) maybeOverlayRow

    paddedOverlayRows = sandwich rowOffset overlayArea
    sandwich offsetNum content = modifyFront $ map Just content
     where
      negatedOffset = fromIntegral offsetNum
      modifyFront =
        if negatedOffset >= 0
          then (replicate negatedOffset Nothing <>)
          else drop $ abs negatedOffset

-- | Overlays all of the "child placements" from beginning to end, such that the
-- latter children supersede the earlier ones.
mergeStructures :: M.Map StructureName (PStructure e) -> PStructure e -> MergedStructure e
mergeStructures inheritedStrucDefs (Structure origArea subStructures subPlacements) =
  foldl' (overlaySingleStructure structureMap) (MergedStructure origArea) overlays
 where
  -- deeper definitions override the outer (toplevel) ones
  structureMap = M.union (M.fromList $ map (name &&& structure) subStructures) inheritedStrucDefs
  overlays = mapMaybe g subPlacements
  g placement@(Placement sName _ _) =
    sequenceA (placement, M.lookup sName structureMap)

type Structure = PStructure Entity

instance FromJSONE (EntityMap, RobotMap) Structure where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    structureDefs <- v ..:? "structures" ..!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    initialArea <- liftE $ (v .:? "map" .!= "") >>= paintMap pal
    return $ Structure initialArea structureDefs placementDefs

data Placement = Placement
  { src :: StructureName
  , ul :: Location
  , up :: AbsoluteDir
  -- ^ TODO: Not implemented yet
  }
  deriving (Eq, Show)

instance FromJSON Placement where
  parseJSON = withObject "structure placement" $ \v -> do
    sName <- v .: "src"
    Placement sName
      <$> (v .:? "upperleft" .!= origin)
      <*> (v .:? "up" .!= DNorth)

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
