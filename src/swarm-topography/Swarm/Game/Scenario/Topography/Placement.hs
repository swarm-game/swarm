{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Descriptions of the orientation and offset by
-- which a structure should be placed.
module Swarm.Game.Scenario.Topography.Placement where

import Data.List.NonEmpty qualified as NE
import Data.Yaml as Y
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area (
  AreaDimensions (..),
 )
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Named (
  StructureName,
 )
import Swarm.Language.Syntax.Direction (AbsoluteDir (..))
import Swarm.Util (applyWhen)

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
    up <- v .:? "up" .!= DNorth
    flipped <- v .:? "flip" .!= False
    pure Orientation {..}

defaultOrientation :: Orientation
defaultOrientation = Orientation DNorth False

-- | This is the point-wise equivalent of "applyOrientationTransform"
reorientLandmark :: Orientation -> AreaDimensions -> Location -> Location
reorientLandmark (Orientation upDir shouldFlip) (AreaDimensions width height) =
  rotational . flipping
 where
  transposeLoc (Location x y) = Location (-y) (-x)
  flipV (Location x y) = Location x $ -(height - 1) - y
  flipH (Location x y) = Location (width - 1 - x) y
  flipping = applyWhen shouldFlip flipV
  rotational = case upDir of
    DNorth -> id
    DSouth -> flipH . flipV
    DEast -> transposeLoc . flipV
    DWest -> transposeLoc . flipH

applyOrientationTransform ::
  Orientation ->
  Grid a ->
  Grid a
applyOrientationTransform _ EmptyGrid = EmptyGrid
applyOrientationTransform f (Grid g) = Grid $ applyOrientationTransformNE f g

-- | affine transformation
applyOrientationTransformNE :: Orientation -> NonEmptyGrid a -> NonEmptyGrid a
applyOrientationTransformNE (Orientation upDir shouldFlip) =
  mapRowsNE f
 where
  f = rotational . flipping
  flipV = NE.reverse
  flipping = applyWhen shouldFlip flipV
  rotational = case upDir of
    DNorth -> id
    DSouth -> NE.transpose . flipV . NE.transpose . flipV
    DEast -> NE.transpose . flipV
    DWest -> flipV . NE.transpose

data Pose = Pose
  { offset :: Location
  , orient :: Orientation
  }
  deriving (Eq, Show)

data Placement = Placement
  { src :: StructureName
  , structurePose :: Pose
  }
  deriving (Eq, Show)

instance FromJSON Placement where
  parseJSON = withObject "structure placement" $ \v -> do
    src <- v .: "src"
    offset <- v .:? "offset" .!= origin
    orient <- v .:? "orient" .!= defaultOrientation
    let structurePose = Pose offset orient
    pure Placement {..}
