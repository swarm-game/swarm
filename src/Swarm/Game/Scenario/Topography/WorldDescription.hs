{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.WorldDescription where

import Data.Coerce
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (
  WaypointName,
 )
import Swarm.Game.Scenario.Topography.Structure (InheritedStructureDefs, MergedStructure (MergedStructure), PStructure (Structure))
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Game.Universe
import Swarm.Util.Yaml

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
  , navigation :: Navigation Identity WaypointName
  , worldName :: SubworldName
  }
  deriving (Eq, Show)

type WorldDescription = PWorldDescription Entity

instance FromJSONE (InheritedStructureDefs, (EntityMap, RobotMap)) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    (scnenarioLevelStructureDefs, (em, rm)) <- getE
    (pal, terr, rootWorldStructureDefs) <- localE (const (em, rm)) $ do
      pal <- v ..:? "palette" ..!= WorldPalette mempty
      terr <- v ..:? "default"
      rootWorldStructs <- v ..:? "structures" ..!= []
      return (pal, terr, rootWorldStructs)

    waypointDefs <- liftE $ v .:? "waypoints" .!= []
    portalDefs <- liftE $ v .:? "portals" .!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    (initialArea, mapWaypoints) <- liftE ((v .:? "map" .!= "") >>= Structure.paintMap Nothing pal)

    upperLeft <- liftE (v .:? "upperleft" .!= origin)
    subWorldName <- liftE (v .:? "name" .!= defaultRootSubworldName)

    let initialStructureDefs = scnenarioLevelStructureDefs <> rootWorldStructureDefs
        struc = Structure initialArea initialStructureDefs placementDefs $ waypointDefs <> mapWaypoints
        MergedStructure mergedArea unmergedWaypoints = Structure.mergeStructures mempty Nothing struc

    validatedNavigation <-
      validatePartialNavigation
        subWorldName
        (coerce upperLeft)
        unmergedWaypoints
        portalDefs

    WorldDescription terr
      <$> liftE (v .:? "offset" .!= False)
      <*> liftE (v .:? "scrollable" .!= True)
      <*> pure pal
      <*> pure upperLeft
      <*> pure (map catMaybes mergedArea) -- Root-level map has no transparent cells.
      <*> pure validatedNavigation
      <*> pure subWorldName

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
