{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Language.Syntax.Direction (AbsoluteDir)
import Swarm.Util (failT, showT)
import Swarm.Util.Yaml
import Witch (into)

data NamedArea a = NamedArea
  { name :: StructureName
  , recognize :: Set AbsoluteDir
  -- ^ whether this structure should be registered for automatic recognition
  -- and which orientations shall be recognized.
  -- The supplied direction indicates which cardinal direction the
  -- original map's "North" has been re-oriented to.
  -- E.g., 'DWest' represents a rotation of 90 degrees counter-clockwise.
  , description :: Maybe Text
  -- ^ will be UI-facing only if this is a recognizable structure
  , structure :: a
  }
  deriving (Eq, Show, Functor)

isRecognizable :: NamedArea a -> Bool
isRecognizable = not . null . recognize

type NamedGrid c = NamedArea (Grid c)

type NamedStructure c = NamedArea (PStructure c)

type InheritedStructureDefs = [NamedStructure (Maybe Cell)]

instance FromJSONE (TerrainEntityMaps, RobotMap) (NamedArea (PStructure (Maybe Cell))) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    NamedArea
      <$> liftE (v .: "name")
      <*> liftE (v .:? "recognize" .!= mempty)
      <*> liftE (v .:? "description")
      <*> v
        ..: "structure"

data PStructure c = Structure
  { area :: Grid c
  , structures :: [NamedStructure c]
  -- ^ structure definitions from parents shall be accessible by children
  , placements :: [Placement]
  -- ^ earlier placements will be overlaid on top of later placements in the YAML file
  , waypoints :: [Waypoint]
  }
  deriving (Eq, Show)

data Placed c = Placed Placement (NamedStructure c)
  deriving (Show)

-- | For use in registering recognizable pre-placed structures
data LocatedStructure = LocatedStructure
  { placedName :: StructureName
  , upDirection :: AbsoluteDir
  , cornerLoc :: Location
  }
  deriving (Show)

instance HasLocation LocatedStructure where
  modifyLoc f (LocatedStructure x y originalLoc) =
    LocatedStructure x y $ f originalLoc

data MergedStructure c = MergedStructure (Grid c) [LocatedStructure] [Originated Waypoint]

instance FromJSONE (TerrainEntityMaps, RobotMap) (PStructure (Maybe Cell)) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    localStructureDefs <- v ..:? "structures" ..!= []

    liftE $ do
      placementDefs <- v .:? "placements" .!= []
      waypointDefs <- v .:? "waypoints" .!= []
      maybeMaskChar <- v .:? "mask"
      (maskedArea, mapWaypoints) <- (v .:? "map" .!= "") >>= paintMap maybeMaskChar pal
      return $
        Structure
          (Grid maskedArea)
          localStructureDefs
          placementDefs
          (waypointDefs <> mapWaypoints)

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
