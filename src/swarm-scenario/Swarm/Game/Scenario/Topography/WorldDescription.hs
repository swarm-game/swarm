{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.WorldDescription where

import Control.Carrier.Reader (runReader)
import Control.Carrier.Throw.Either
import Control.Monad (forM)
import Data.Coerce
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (
  Parentage (Root),
  WaypointName,
 )
import Swarm.Game.Scenario.Topography.Structure (
  InheritedStructureDefs,
  LocatedStructure,
  MergedStructure (MergedStructure),
  PStructure (Structure),
 )
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly qualified as Assembly
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Game.Universe
import Swarm.Game.World.Parse ()
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck
import Swarm.Language.Pretty (prettyString)
import Swarm.Util.Yaml

------------------------------------------------------------
-- World description
------------------------------------------------------------

-- | A description of a world parsed from a YAML file.
-- This type is parameterized to accommodate Cells that
-- utilize a less stateful Entity type.
data PWorldDescription e = WorldDescription
  { offsetOrigin :: Bool
  , scrollable :: Bool
  , palette :: WorldPalette e
  , ul :: Location
  , area :: [[PCell e]]
  , navigation :: Navigation Identity WaypointName
  , placedStructures :: [LocatedStructure]
  , worldName :: SubworldName
  , worldProg :: Maybe (TTerm '[] (World CellVal))
  }
  deriving (Show)

type WorldDescription = PWorldDescription Entity

data WorldParseDependencies
  = WorldParseDependencies
      WorldMap
      InheritedStructureDefs
      RobotMap
      -- | last for the benefit of partial application
      TerrainEntityMaps

instance FromJSONE WorldParseDependencies WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    WorldParseDependencies worldMap scenarioLevelStructureDefs rm tem <- getE
    (pal, rootWorldStructureDefs) <- localE (const (tem, rm)) $ do
      pal <- v ..:? "palette" ..!= WorldPalette mempty
      rootWorldStructs <- v ..:? "structures" ..!= []
      return (pal, rootWorldStructs)

    waypointDefs <- liftE $ v .:? "waypoints" .!= []
    portalDefs <- liftE $ v .:? "portals" .!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    (initialArea, mapWaypoints) <- liftE ((v .:? "map" .!= "") >>= Structure.paintMap Nothing pal)

    upperLeft <- liftE (v .:? "upperleft" .!= origin)
    subWorldName <- liftE (v .:? "name" .!= DefaultRootSubworld)

    let initialStructureDefs = scenarioLevelStructureDefs <> rootWorldStructureDefs
        struc = Structure initialArea initialStructureDefs placementDefs $ waypointDefs <> mapWaypoints

    MergedStructure mergedArea staticStructurePlacements unmergedWaypoints <-
      either (fail . T.unpack) return $
        Assembly.mergeStructures mempty Root struc

    let absoluteStructurePlacements =
          map (offsetLoc $ coerce upperLeft) staticStructurePlacements

    validatedNavigation <-
      validatePartialNavigation
        subWorldName
        upperLeft
        unmergedWaypoints
        portalDefs

    mwexp <- liftE (v .:? "dsl")
    dslTerm <- forM mwexp $ \wexp -> do
      let checkResult =
            run . runThrow @CheckErr . runReader worldMap . runReader tem $
              check CNil (TTyWorld TTyCell) wexp
      either (fail . prettyString) return checkResult
    WorldDescription
      <$> liftE (v .:? "offset" .!= False)
      <*> liftE (v .:? "scrollable" .!= True)
      <*> pure pal
      <*> pure upperLeft
      <*> pure (map catMaybes mergedArea) -- Root-level map has no transparent cells.
      <*> pure validatedNavigation
      <*> pure absoluteStructurePlacements
      <*> pure subWorldName
      <*> pure dslTerm

------------------------------------------------------------
-- World editor
------------------------------------------------------------

-- | A pared-down (stateless) version of "WorldDescription" just for
-- the purpose of rendering a Scenario file
type WorldDescriptionPaint = PWorldDescription EntityFacade

instance ToJSON WorldDescriptionPaint where
  toJSON w =
    object
      [ "offset" .= offsetOrigin w
      , "palette" .= Y.toJSON paletteKeymap
      , "upperleft" .= ul w
      , "map" .= Y.toJSON mapText
      ]
   where
    cellGrid = area w
    suggestedPalette = palette w
    (mapText, paletteKeymap) = prepForJson suggestedPalette cellGrid
