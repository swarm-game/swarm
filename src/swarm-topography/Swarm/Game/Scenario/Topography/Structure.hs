{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure where

import Control.Monad (unless)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.ProtoCell
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.World.Coords
import Swarm.Language.Syntax.Direction (AbsoluteDir)
import Swarm.Util (failT, showT)
import Swarm.Util.Yaml

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
    case NE.nonEmpty textLines of
      Nothing -> return emptyGrid
      Just nonemptyRows -> do
        let firstRowLength = length $ NE.head nonemptyRows
        unless (all ((== firstRowLength) . length) $ NE.tail nonemptyRows) $
          fail "Grid is not rectangular!"
        return $ Grid textLines

instance (FromJSONE e a) => FromJSONE e (PStructure (Maybe a)) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= StructurePalette mempty
    structures <- v ..:? "structures" ..!= []
    liftE $ do
      placements <- v .:? "placements" .!= []
      waypointDefs <- v .:? "waypoints" .!= []
      maybeMaskChar <- v .:? "mask"
      rawGrid <- v .:? "map" .!= emptyGrid
      (maskedArea, mapWaypoints) <- paintMap maybeMaskChar pal rawGrid
      let area = PositionedGrid origin maskedArea
          waypoints = waypointDefs <> mapWaypoints
      return Structure {..}

-- | \"Paint\" a world map using a 'WorldPalette', turning it from a raw
--   string into a nested list of 'PCell' values by looking up each
--   character in the palette, failing if any character in the raw map
--   is not contained in the palette.
paintMap ::
  MonadFail m =>
  Maybe Char ->
  StructurePalette c ->
  Grid Char ->
  m (Grid (Maybe c), [Waypoint])
paintMap maskChar pal g = do
  nestedLists <- mapM toCell g
  let usedChars = Set.fromList $ map T.singleton $ allMembers g
      unusedChars =
        filter (`Set.notMember` usedChars)
          . M.keys
          . KeyMap.toMapText
          $ unPalette pal

  unless (null unusedChars) $
    fail $
      unwords
        [ "Unused characters in palette:"
        , T.unpack $ T.intercalate ", " unusedChars
        ]

  let cells = fmap standardCell <$> nestedLists
      getWp coords maybeAugmentedCell = do
        wpCfg <- waypointCfg =<< maybeAugmentedCell
        return . Waypoint wpCfg . coordsToLoc $ coords
      wps = catMaybes $ mapIndexedMembers getWp nestedLists

  return (cells, wps)
 where
  toCell c =
    if Just c == maskChar
      then return Nothing
      else case KeyMap.lookup (Key.fromString [c]) (unPalette pal) of
        Nothing -> failT ["Char not in world palette:", showT c]
        Just cell -> return $ Just cell
