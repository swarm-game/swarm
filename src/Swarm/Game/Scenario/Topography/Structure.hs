{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map,
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure where

import Control.Applicative ((<|>))
import Control.Arrow (left, (&&&))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Coerce
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldrM)
import Data.Int (Int32)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml as Y
import Linear (V2)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Util (failT, quote, showT)
import Swarm.Util.Yaml
import Witch (into)

data NamedStructure c = NamedStructure
  { name :: StructureName
  , recognize :: Bool
  -- ^ whether this structure should be registered for automatic recognition
  , structure :: PStructure c
  }
  deriving (Eq, Show)

type InheritedStructureDefs = [NamedStructure (Maybe Cell)]

instance FromJSONE (EntityMap, RobotMap) (NamedStructure (Maybe Cell)) where
  parseJSONE = withObjectE "named structure" $ \v -> do
    NamedStructure
      <$> liftE (v .: "name")
      <*> liftE (v .:? "recognize" .!= False)
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

data Placed c = Placed Placement (PStructure c)
  deriving (Show)

-- | For use in registering recognizable pre-placed structures
data LocatedStructure c = LocatedStructure
  { originalPlacement :: Placed c
  , absoluteCornerLoc :: Location
  }
  deriving (Show)

-- | Translation by a vector
offsetLocatedStructure ::
  V2 Int32 ->
  LocatedStructure c ->
  LocatedStructure c
offsetLocatedStructure locOffset = modifyLocatedStructureLocation (.+^ locOffset)

-- | Basically "fmap" for the "Location" field
modifyLocatedStructureLocation ::
  (Location -> Location) ->
  LocatedStructure c ->
  LocatedStructure c
modifyLocatedStructureLocation f (LocatedStructure x originalLoc) =
  LocatedStructure x $ f originalLoc

data MergedStructure c = MergedStructure [[c]] [LocatedStructure c] [Originated Waypoint]

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
overlaySingleStructure ::
  M.Map StructureName (PStructure (Maybe a)) ->
  Placed (Maybe a) ->
  MergedStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
overlaySingleStructure
  inheritedStrucDefs
  (Placed p@(Placement _ loc@(Location colOffset rowOffset) orientation) struc)
  (MergedStructure inputArea inputPlacements inputWaypoints) = do
    MergedStructure overlayArea overlayPlacements overlayWaypoints <-
      mergeStructures inheritedStrucDefs (WithParent p) struc

    let mergedWaypoints = inputWaypoints <> map (fmap $ placeWaypoint overlayArea) overlayWaypoints

        mergedPlacements = inputPlacements <> map (placeStructureTemplate overlayArea) overlayPlacements

        mergedArea = zipWithPad mergeSingleRow inputArea $ paddedOverlayRows overlayArea

    return $ MergedStructure mergedArea mergedPlacements mergedWaypoints
   where
    placeStructureTemplate overArea =
      offsetLocatedStructure (coerce loc)
        . modifyLocatedStructureLocation (reorientLandmark orientation $ getAreaDimensions overArea)

    placeWaypoint overArea =
      offsetWaypoint (coerce loc)
        . modifyLocation (reorientLandmark orientation $ getAreaDimensions overArea)

    zipWithPad f a b = zipWith f a $ b <> repeat Nothing

    affineTransformedOverlay = applyOrientationTransform orientation

    mergeSingleRow inputRow maybeOverlayRow =
      zipWithPad (flip (<|>)) inputRow paddedSingleOverlayRow
     where
      paddedSingleOverlayRow = maybe [] (applyOffset colOffset) maybeOverlayRow

    paddedOverlayRows = applyOffset (negate rowOffset) . map Just . affineTransformedOverlay
    applyOffset offsetNum = modifyFront
     where
      integralOffset = fromIntegral offsetNum
      modifyFront =
        if integralOffset >= 0
          then (replicate integralOffset Nothing <>)
          else drop $ abs integralOffset

elaboratePlacement :: Parentage Placement -> Either Text a -> Either Text a
elaboratePlacement p = left (elaboration <>)
 where
  pTxt = case p of
    Root -> "root placement"
    WithParent (Placement (StructureName sn) loc _) ->
      T.unwords
        [ "placement of"
        , quote sn
        , "at"
        , showT loc
        ]
  elaboration =
    T.unwords
      [ "Within"
      , pTxt <> ":"
      , ""
      ]

-- | Overlays all of the "child placements", such that the children encountered earlier
-- in the YAML file supersede the later ones (due to use of 'foldr' instead of 'foldl').
mergeStructures ::
  M.Map StructureName (PStructure (Maybe a)) ->
  Parentage Placement ->
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
mergeStructures inheritedStrucDefs parentPlacement (Structure origArea subStructures subPlacements subWaypoints) = do
  overlays <- elaboratePlacement parentPlacement $ mapM g subPlacements
  let wrapPlacement p@(Placed z _) = LocatedStructure p $ offset z
      wrappedOverlays = map wrapPlacement overlays
  foldrM
    (overlaySingleStructure structureMap)
    (MergedStructure origArea wrappedOverlays originatedWaypoints)
    overlays
 where
  originatedWaypoints = map (Originated parentPlacement) subWaypoints

  -- deeper definitions override the outer (toplevel) ones
  structureMap = M.union (M.fromList $ map (name &&& structure) subStructures) inheritedStrucDefs

  g placement@(Placement sName@(StructureName n) _ _) =
    maybeToEither
      (T.unwords ["Could not look up structure", quote n])
      $ uncurry Placed <$> sequenceA (placement, M.lookup sName structureMap)

instance FromJSONE (EntityMap, RobotMap) (PStructure (Maybe Cell)) where
  parseJSONE = withObjectE "structure definition" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    localStructureDefs <- v ..:? "structures" ..!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    waypointDefs <- liftE $ v .:? "waypoints" .!= []
    maybeMaskChar <- liftE $ v .:? "mask"
    (maskedArea, mapWaypoints) <- liftE $ (v .:? "map" .!= "") >>= paintMap maybeMaskChar pal
    return $ Structure maskedArea localStructureDefs placementDefs $ waypointDefs <> mapWaypoints

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
