{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup (RobotMap)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.Structure.Type
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Util (failT, showT)
import Swarm.Util.Yaml
import Witch (into)

type InheritedStructureDefs = [NamedStructure (Maybe Cell)]

instance FromJSONE (TerrainEntityMaps, RobotMap) (NamedArea (PStructure (Maybe Cell))) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    NamedArea
      <$> liftE (v .: "name")
      <*> liftE (v .:? "recognize" .!= mempty)
      <*> liftE (v .:? "description")
      <*> v
        ..: "structure"

instance FromJSONE (TerrainEntityMaps, RobotMap) (PStructure (Maybe Cell)) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    structures <- v ..:? "structures" ..!= []

    liftE $ do
      placements <- v .:? "placements" .!= []
      waypointDefs <- v .:? "waypoints" .!= []
      maybeMaskChar <- v .:? "mask"
      (maskedArea, mapWaypoints) <- (v .:? "map" .!= "") >>= paintMap maybeMaskChar pal
      let area = PositionedGrid origin $ Grid maskedArea
          waypoints = waypointDefs <> mapWaypoints
      return Structure {..}

-- | \"Paint\" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'PCell' values by looking up each
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
