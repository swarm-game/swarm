{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map,
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Coerce
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Util (failT, showT)
import Swarm.Util.Yaml
import Witch (into)

data NamedStructure c = NamedStructure
  { name :: StructureName
  , structure :: PStructure c
  }
  deriving (Eq, Show)

type InheritedStructureDefs = [NamedStructure (Maybe (PCell Entity))]

instance FromJSONE (EntityMap, RobotMap) (NamedStructure (Maybe (PCell Entity))) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    NamedStructure
      <$> liftE (v .: "name")
      <*> v
        ..: "structure"

data PStructure c = Structure
  { area :: [[c]]
  , structures :: [NamedStructure c]
  -- ^ structure definitions from parents shall be accessible by children
  , placements :: [Placement]
  -- ^ earlier placements will be overlaid on top of later placements in the YAML file
  , waypoints :: [Waypoint]
  }
  deriving (Eq, Show)

data MergedStructure c = MergedStructure [[c]] [Originated Waypoint]

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
  (p@(Placement _ loc@(Location colOffset rowOffset) orientation), struc)
  (MergedStructure inputArea inputWaypoints) =
    MergedStructure mergedArea mergedWaypoints
   where
    mergedArea = zipWithPad mergeSingleRow inputArea paddedOverlayRows

    placeWaypoint =
      offsetWaypoint (coerce loc)
        . modifyLocation (reorientWaypoint orientation $ getAreaDimensions overlayArea)
    mergedWaypoints = inputWaypoints <> map (fmap placeWaypoint) overlayWaypoints

    zipWithPad f a b = zipWith f a $ b <> repeat Nothing

    MergedStructure overlayArea overlayWaypoints = mergeStructures inheritedStrucDefs (Just p) struc
    affineTransformedOverlay = applyOrientationTransform orientation overlayArea

    mergeSingleRow inputRow maybeOverlayRow =
      zipWithPad (flip (<|>)) inputRow paddedSingleOverlayRow
     where
      paddedSingleOverlayRow = maybe [] (applyOffset colOffset) maybeOverlayRow

    paddedOverlayRows = applyOffset (negate rowOffset) . map Just $ affineTransformedOverlay
    applyOffset offsetNum = modifyFront
     where
      integralOffset = fromIntegral offsetNum
      modifyFront =
        if integralOffset >= 0
          then (replicate integralOffset Nothing <>)
          else drop $ abs integralOffset

-- | Overlays all of the "child placements", such that the children encountered earlier
-- in the YAML file supersede the later ones (due to use of 'foldr' instead of 'foldl').
mergeStructures ::
  M.Map StructureName (PStructure (Maybe a)) ->
  Maybe Placement ->
  PStructure (Maybe a) ->
  MergedStructure (Maybe a)
mergeStructures inheritedStrucDefs parentPlacement (Structure origArea subStructures subPlacements subWaypoints) =
  foldr (overlaySingleStructure structureMap) (MergedStructure origArea originatedWaypoints) overlays
 where
  originatedWaypoints = map (Originated parentPlacement) subWaypoints

  -- deeper definitions override the outer (toplevel) ones
  structureMap = M.union (M.fromList $ map (name &&& structure) subStructures) inheritedStrucDefs
  overlays = mapMaybe g subPlacements
  g placement@(Placement sName _ _) =
    sequenceA (placement, M.lookup sName structureMap)

instance FromJSONE (EntityMap, RobotMap) (PStructure (Maybe (PCell Entity))) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    localStructureDefs <- v ..:? "structures" ..!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    waypointDefs <- liftE $ v .:? "waypoints" .!= []
    maybeMaskChar <- liftE $ v .:? "mask"
    (maskedArea, mapWaypoints) <- liftE $ (v .:? "map" .!= "") >>= paintMap maybeMaskChar pal
    return $ Structure maskedArea localStructureDefs placementDefs $ waypointDefs <> mapWaypoints

-- | \"Paint\" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'Cell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap ::
  MonadFail m =>
  Maybe Char ->
  WorldPalette e ->
  Text ->
  m ([[Maybe (PCell e)]], [Waypoint])
paintMap maskChar pal a = do
  nestedLists <- readMap toCell a
  let cells = map (map $ fmap standardCell) nestedLists
      f i j maybeAugmentedCell = do
        wpCfg <- waypointCfg =<< maybeAugmentedCell
        return . Waypoint wpCfg . Location j $ negate i
      wps = concat $ zipWith (\i -> catMaybes . zipWith (f i) [0 ..]) [0 ..] nestedLists

  return (cells, wps)
 where
  toCell c =
    if Just c == maskChar
      then return Nothing
      else case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
        Nothing -> failT ["Char not in world palette:", showT c]
        Just cell -> return $ Just cell

readMap :: Applicative f => (Char -> f b) -> Text -> f [[b]]
readMap func = traverse (traverse func . into @String) . T.lines
