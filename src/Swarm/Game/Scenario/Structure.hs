{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Structure where

import Linear (V2 (..))
import Data.Int (Int32)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Coerce
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

newtype WaypointName = WaypointName Text
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- | Indicates which structure something came from
-- for debugging purposes.
data Originated a = Originated
  { parent :: Maybe Placement
  , value :: a
  }
  deriving (Show, Eq, Functor)

-- |
-- A parent world shouldn't have to know the exact layout of a subworld
-- to specify where exactly a portal will deliver a robot to within the subworld.
-- Therefore, we define named waypoints in the subworld and the parent world
-- must reference them by name, rather than by coordinate.
data Waypoint = Waypoint
  { wpName :: WaypointName
    -- | Enforce global uniqueness of this waypoint
  , wpUnique :: Bool
  , wpLoc :: Location
  }
  deriving (Show, Eq)

instance FromJSON Waypoint where
  parseJSON = withObject "Waypoint" $ \v ->
    Waypoint
      <$> v .: "name"
      <*> v .:? "unique" .!= False
      <*> v .: "loc"

offsetWaypoint :: V2 Int32
  -> Waypoint -> Waypoint
offsetWaypoint locOffset (Waypoint n u originalLoc) = Waypoint n u $ originalLoc .+^ locOffset

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
  (p@(Placement _ loc@(Location colOffset rowOffset) orientation), struc)
  (MergedStructure inputArea inputWaypoints) =
    MergedStructure mergedArea mergedWaypoints
   where
    mergedArea = zipWithPad mergeSingleRow inputArea paddedOverlayRows

    mergedWaypoints = inputWaypoints ++ map (fmap $ offsetWaypoint $ coerce loc) overlayWaypoints

    zipWithPad f a b = zipWith f a $ b <> repeat Nothing

    MergedStructure overlayArea overlayWaypoints = mergeStructures inheritedStrucDefs (Just p) struc
    affineTransformedOverlay = getTransform orientation overlayArea

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
-- in the YAML file supersede the later ones (due to use of "foldr" instead of "foldl").
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
    structureDefs <- v ..:? "structures" ..!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    waypointDefs <- liftE $ v .:? "waypoints" .!= []
    maybeMaskChar <- liftE $ v .:? "mask"
    maskedArea <- liftE $ (v .:? "map" .!= "") >>= paintMap maybeMaskChar pal
    return $ Structure maskedArea structureDefs placementDefs waypointDefs

-- | affine transformation
getTransform :: Orientation -> ([[a]] -> [[a]])
getTransform (Orientation upDir shouldFlip) =
  rotational . flipping
 where
  flipV = reverse
  flipping = if shouldFlip then flipV else id
  rotational = case upDir of
    DNorth -> id
    DSouth -> transpose . flipV . transpose . flipV
    DEast -> transpose . flipV
    DWest -> flipV . transpose

data Placement = Placement
  { src :: StructureName
  , offset :: Location
  , orient :: Orientation
  }
  deriving (Show, Eq)

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
