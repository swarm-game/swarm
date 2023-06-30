{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.WorldDescription where

import Control.Monad (forM)
import Data.Coerce
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.Scenario.Portal
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Structure qualified as Structure
import Swarm.Game.Scenario.WorldPalette
import Swarm.Util (quote)
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
  , waypoints :: M.Map Structure.WaypointName Location
  -- ^ Note that waypoints defined at the "root" level are still relative to
  -- the top-left corner of the map rectangle; they are not in absolute world
  -- coordinates (as with applying the "ul" offset).
  , portals :: M.Map Location Location
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
    initialArea <- liftE ((v .:? "map" .!= "") >>= Structure.paintMap Nothing pal)

    upperLeft <- liftE (v .:? "upperleft" .!= origin)

    let struc = Structure.Structure initialArea structureDefs placementDefs waypointDefs
        Structure.MergedStructure mergedArea unmergedWaypoints = Structure.mergeStructures mempty Nothing struc

    -- TODO: Throw error upon overwrites
    -- TODO: Add unit test for parse validation
    let mergedWaypoints = M.fromList $ map (\(Structure.Originated _ (Structure.Waypoint n _ loc)) -> (n, loc)) unmergedWaypoints
        correctedWaypoints = M.map (.+^ coerce upperLeft) mergedWaypoints

    -- TODO Currently ignores subworld references
    reconciledPortalPairs <- forM portalDefs $ \(Portal entranceName (PortalExit exitName _)) -> do
      let getLoc wpName@(Structure.WaypointName rawName) = case M.lookup wpName correctedWaypoints of
            Nothing -> fail $ T.unpack $ T.unwords ["No waypoint named", quote rawName]
            Just x -> return x
      entranceLoc <- getLoc entranceName
      exitLoc <- getLoc exitName
      return (entranceLoc, exitLoc)

    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> liftE (v .:? "scrollable" .!= True)
      <*> pure pal
      <*> pure upperLeft
      <*> pure (map catMaybes mergedArea) -- Root-level map has no transparent cells.
      <*> pure correctedWaypoints
      <*> pure (M.fromList reconciledPortalPairs)

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
