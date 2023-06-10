{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Structure where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (transpose)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.WorldPalette
import Swarm.Language.Syntax (AbsoluteDir (..))
import Swarm.Util.Yaml
import Witch (into)

newtype StructureName = StructureName Text
  deriving (Eq, Ord, Show, Generic, FromJSON)

data NamedStructure c = NamedStructure
  { name :: StructureName
  , structure :: PStructure c
  }
  deriving (Eq, Show)

instance FromJSONE (EntityMap, RobotMap) (NamedStructure (Maybe (PCell Entity))) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    sName <- liftE $ v .: "name"
    NamedStructure sName
      <$> v
      ..: "structure"

type MaskedArea c = [[Maybe c]]

data PStructure c = Structure
  { -- area :: MaskedArea c
    area :: [[c]]
  , structures :: [NamedStructure c]
  -- ^ structure definitions from parents shall be accessible by children
  , placements :: [Placement]
  }
  deriving (Eq, Show)

newtype MergedStructure c = MergedStructure [[c]]

data Orientation = Orientation
  { up :: AbsoluteDir
  , flipped :: Bool
  -- ^ vertical flip, applied before rotation
  }
  deriving (Eq, Show)

instance FromJSON Orientation where
  parseJSON = withObject "structure orientation" $ \v -> do
    Orientation
      <$> (v .:? "up" .!= DNorth)
      <*> (v .:? "flip" .!= False)

defaultOrientation :: Orientation
defaultOrientation = Orientation DNorth False

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
overlaySingleStructure ::
  M.Map StructureName (PStructure (Maybe a)) ->
  (Placement, PStructure (Maybe a)) ->
  MergedStructure (Maybe a) ->
  MergedStructure (Maybe a)
overlaySingleStructure
  inheritedStrucDefs
  (Placement _ (Location colOffset rowOffset) orientation, struc)
  (MergedStructure inputArea) =
    MergedStructure $ zipWithPad mergeSingleRow inputArea paddedOverlayRows
   where
    zipWithPad f a b = zipWith f a $ b <> repeat Nothing
    MergedStructure overlayArea = mergeStructures inheritedStrucDefs struc
    affineTransformedOverlay = getTransform orientation overlayArea

    mergeSingleRow inputRow maybeOverlayRow =
      zipWithPad (flip (<|>)) inputRow paddedSingleOverlayRow
     where
      paddedSingleOverlayRow = maybe [] (applyOffset id colOffset) maybeOverlayRow

    paddedOverlayRows = applyOffset (map Just) (negate rowOffset) affineTransformedOverlay
    applyOffset txform offsetNum = modifyFront . txform
     where
      negatedOffset = fromIntegral offsetNum
      modifyFront =
        if negatedOffset >= 0
          then (replicate negatedOffset Nothing <>)
          else drop $ abs negatedOffset

-- | Overlays all of the "child placements" from beginning to end, such that the
-- latter children supersede the earlier ones.
mergeStructures :: M.Map StructureName (PStructure (Maybe a)) -> PStructure (Maybe a) -> MergedStructure (Maybe a)
mergeStructures inheritedStrucDefs (Structure origArea subStructures subPlacements) =
  foldr (overlaySingleStructure structureMap) (MergedStructure origArea) overlays
 where
  -- deeper definitions override the outer (toplevel) ones
  structureMap = M.union (M.fromList $ map (name &&& structure) subStructures) inheritedStrucDefs
  overlays = mapMaybe g subPlacements
  g placement@(Placement sName _ _) =
    sequenceA (placement, M.lookup sName structureMap)

instance FromJSONE (EntityMap, RobotMap) (PStructure (Maybe (PCell Entity))) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    structureDefs <- v ..:? "structures" ..!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    maybeMaskChar <- liftE $ v .:? "mask"
    maskedArea <- liftE $ (v .:? "map" .!= "") >>= paintMap maybeMaskChar pal
    return $ Structure maskedArea structureDefs placementDefs

-- | affine transformation
getTransform :: Orientation -> ([[a]] -> [[a]])
getTransform (Orientation upDir shouldFlip) =
  rotational . flipping
 where
  flipping = if shouldFlip then flipV else id
  rotational = case upDir of
    DNorth -> id
    DSouth -> flipV . flipH
    DEast -> transpose . flipV
    DWest -> flipV . transpose

  flipV = reverse
  flipH = map reverse

data Placement = Placement
  { src :: StructureName
  , offset :: Location
  , orient :: Orientation
  }
  deriving (Eq, Show)

instance FromJSON Placement where
  parseJSON = withObject "structure placement" $ \v -> do
    sName <- v .: "src"
    Placement sName
      <$> (v .:? "offset" .!= origin)
      <*> (v .:? "orient" .!= defaultOrientation)

-- | "Paint" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'Cell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap :: MonadFail m => Maybe Char -> WorldPalette e -> Text -> m [[Maybe (PCell e)]]
paintMap maskChar pal = readMap toCell
 where
  toCell c =
    if Just c == maskChar
      then return Nothing
      else case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
        Nothing -> fail $ "Char not in world palette: " ++ show c
        Just cell -> return $ Just cell

readMap :: Applicative f => (Char -> f b) -> Text -> f [[b]]
readMap func = traverse (traverse func . into @String) . T.lines
