{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Descriptions of the orientation and offset by
-- which a structure should be placed.
module Swarm.Game.Scenario.Topography.Placement where

import Data.List (transpose)
import Data.Text (Text)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Language.Syntax (AbsoluteDir (..))

newtype StructureName = StructureName Text
  deriving (Eq, Ord, Show, Generic, FromJSON)

-- | Orientation transformations are applied before translation.
data Orientation = Orientation
  { up :: AbsoluteDir
  -- ^ e.g. For "East", rotates 270 degrees.
  , flipped :: Bool
  -- ^ vertical flip, applied before rotation
  }
  deriving (Eq, Show)

instance FromJSON Orientation where
  parseJSON = withObject "structure orientation" $ \v -> do
    Orientation
      <$> v .:? "up" .!= DNorth
      <*> v .:? "flip" .!= False

defaultOrientation :: Orientation
defaultOrientation = Orientation DNorth False

-- | This is the point-wise equivalent of "applyOrientationTransform"
reorientWaypoint :: Orientation -> AreaDimensions -> Location -> Location
reorientWaypoint (Orientation upDir shouldFlip) (AreaDimensions width height) =
  rotational . flipping
 where
  transposeLoc (Location x y) = Location (-y) (-x)
  flipV (Location x y) = Location x $ -(height - 1) - y
  flipH (Location x y) = Location (width - 1 - x) y
  flipping = if shouldFlip then flipV else id
  rotational = case upDir of
    DNorth -> id
    DSouth -> flipH . flipV
    DEast -> transposeLoc . flipV
    DWest -> transposeLoc . flipH

-- | affine transformation
applyOrientationTransform :: Orientation -> [[a]] -> [[a]]
applyOrientationTransform (Orientation upDir shouldFlip) =
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
  deriving (Eq, Show)

instance FromJSON Placement where
  parseJSON = withObject "structure placement" $ \v -> do
    sName <- v .: "src"
    Placement sName
      <$> v .:? "offset" .!= origin
      <*> v .:? "orient" .!= defaultOrientation
