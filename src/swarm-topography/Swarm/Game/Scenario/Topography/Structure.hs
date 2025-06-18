{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure where

import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Palette
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Game.World.Coords
import Swarm.Util (failT, showT)
import Swarm.Util.Yaml

type NamedStructure c = NamedArea (PStructure c)

data PStructure c = Structure
  { area :: PositionedGrid c
  , structures :: [NamedStructure c]
  -- ^ structure definitions from parents shall be accessible by children
  , placements :: [Placement]
  -- ^ earlier placements will be overlaid on top of later placements in the YAML file
  , waypoints :: [Waypoint]
  }
  deriving (Eq, Show)

data Placed c = Placed Placement (NamedStructure c)
  deriving (Show)

data MergedStructure c = MergedStructure (PositionedGrid c) [LocatedStructure] [Originated Waypoint]

instance (FromJSONE e a) => FromJSONE e (NamedStructure (Maybe a)) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    structure <- v ..: "structure"
    liftE $ do
      name <- v .: "name"
      recognize <- v .:? "recognize" .!= mempty
      description <- v .:? "description"
      return $ NamedArea {..}

instance FromJSON (Grid Char) where
  parseJSON = withText "area" $ \t -> do
    let textLines = map T.unpack $ T.lines t
        g = mkGrid textLines
    case NE.nonEmpty textLines of
      Nothing -> return EmptyGrid
      Just nonemptyRows -> do
        let firstRowLength = length $ NE.head nonemptyRows
        unless (all ((== firstRowLength) . length) $ NE.tail nonemptyRows) $
          fail "Grid is not rectangular!"
        return g

parseStructure ::
  StructurePalette c ->
  [NamedStructure (Maybe c)] ->
  Object ->
  Parser (PStructure (Maybe c))
parseStructure pal structures v = do
  explicitPlacements <- v .:? "placements" .!= []
  waypointDefs <- v .:? "waypoints" .!= []
  maybeMaskChar <- v .:? "mask"
  rawGrid <- v .:? "map" .!= EmptyGrid
  (maskedArea, mapWaypoints, palettePlacements) <- paintMap maybeMaskChar pal rawGrid
  let area = PositionedGrid origin maskedArea
      waypoints = waypointDefs <> mapWaypoints
      placements = explicitPlacements <> palettePlacements
  return Structure {..}

instance (FromJSONE e a) => FromJSONE e (PStructure (Maybe a)) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= StructurePalette mempty mempty
    structures <- v ..:? "structures" ..!= []
    liftE $ parseStructure pal structures v

-- | \"Paint\" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'PCell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap ::
  MonadFail m =>
  Maybe Char ->
  StructurePalette c ->
  Grid Char ->
  m (Grid (Maybe c), [Waypoint], [Placement])
paintMap maskChar pal g = do
  nestedLists <- mapM toCell g
  forM_ maskChar $ \c ->
    unless (Set.notMember c paletteKeys) $
      fail $
        unwords
          [ "Mask character"
          , ['"', c, '"']
          , "overlaps palette entry"
          ]

  unless (Set.null unusedPaletteChars) $
    fail $
      unwords
        [ "Unused characters in palette:"
        , intercalate ", " $ map pure $ Set.toList unusedPaletteChars
        ]

  let cells = fmap standardCell <$> nestedLists
      wps = catMaybes $ mapWithCoords getWp nestedLists

  let extraPlacements =
        catMaybes $ mapWithCoords getStructureMarker nestedLists

  return (cells, wps, extraPlacements)
 where
  getStructureMarker coords maybeAugmentedCell = do
    StructureMarker sName orientation <- structureMarker =<< maybeAugmentedCell
    return
      . Placement sName
      . Pose (coordsToLoc coords)
      $ fromMaybe defaultOrientation orientation

  getWp coords maybeAugmentedCell = do
    wpCfg <- waypointCfg =<< maybeAugmentedCell
    return . Waypoint wpCfg . coordsToLoc $ coords

  usedChars = Set.fromList $ allMembers g
  paletteKeys = M.keysSet $ unPalette pal
  unusedPaletteChars = Set.difference paletteKeys (usedChars <> paletteChars pal)

  toCell c =
    if Just c == maskChar
      then return Nothing
      else case M.lookup c (unPalette pal) of
        Nothing -> failT ["Char not in world palette:", showT c]
        Just cell -> return $ Just cell
