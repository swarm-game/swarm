{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.WorldDescription where

import Data.Coerce
import Data.Maybe (catMaybes)
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Util.Yaml
import Swarm.Game.Universe

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A description of a world parsed from a YAML file.
-- This type is parameterized to accommodate Cells that
-- utilize a less stateful Entity type.
data PWorldDescription e = WorldDescription
  { defaultTerrain :: Maybe (PCell e)
  , offsetOrigin :: Bool
  , scrollable :: Bool
  , palette :: WorldPalette e
  , ul :: Location
  , area :: [[PCell e]]
  , navigation :: Navigation
  }
  deriving (Eq, Show)

type WorldDescription = PWorldDescription Entity

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    structureDefs <- v ..:? "structures" ..!= []
    waypointDefs <- liftE $ v .:? "waypoints" .!= []
    portalDefs <- liftE $ v .:? "portals" .!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    (initialArea, mapWaypoints) <- liftE ((v .:? "map" .!= "") >>= Structure.paintMap Nothing pal)

    upperLeft <- liftE (v .:? "upperleft" .!= origin)

    let struc = Structure.Structure initialArea structureDefs placementDefs $ waypointDefs <> mapWaypoints
        Structure.MergedStructure mergedArea unmergedWaypoints = Structure.mergeStructures mempty Nothing struc

    validatedLandmarks <- validateNavigation
      rootSubworldName -- TODO: Replace with actual subworld name
      (coerce upperLeft)
      unmergedWaypoints
      portalDefs

    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> liftE (v .:? "scrollable" .!= True)
      <*> pure pal
      <*> pure upperLeft
      <*> pure (map catMaybes mergedArea) -- Root-level map has no transparent cells.
      <*> pure validatedLandmarks

------------------------------------------------------------
-- World editor
------------------------------------------------------------

-- | A pared-down (stateless) version of "WorldDescription" just for
-- the purpose of rendering a Scenario file
type WorldDescriptionPaint = PWorldDescription EntityFacade

instance ToJSON WorldDescriptionPaint where
  toJSON w =
    object
      [ "default" .= defaultTerrain w
      , "offset" .= offsetOrigin w
      , "palette" .= Y.toJSON paletteKeymap
      , "upperleft" .= ul w
      , "map" .= Y.toJSON mapText
      ]
   where
    cellGrid = area w
    suggestedPalette = palette w
    (mapText, paletteKeymap) = prepForJson suggestedPalette cellGrid
