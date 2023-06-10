{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.WorldDescription where

import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Structure qualified as Structure
import Swarm.Game.Scenario.WorldPalette
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
  }
  deriving (Eq, Show)

type WorldDescription = PWorldDescription Entity

instance FromJSONE (EntityMap, RobotMap) WorldDescription where
  parseJSONE = withObjectE "world description" $ \v -> do
    pal <- v ..:? "palette" ..!= WorldPalette mempty
    structureDefs <- v ..:? "structures" ..!= []
    placementDefs <- liftE $ v .:? "placements" .!= []
    initialArea <- liftE ((v .:? "map" .!= "") >>= Structure.paintMap pal)

    let struc = Structure.Structure initialArea structureDefs placementDefs
        Structure.MergedStructure mergedArea = Structure.mergeStructures mempty struc

    WorldDescription
      <$> v ..:? "default"
      <*> liftE (v .:? "offset" .!= False)
      <*> liftE (v .:? "scrollable" .!= True)
      <*> pure pal
      <*> liftE (v .:? "upperleft" .!= origin)
      <*> pure mergedArea

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
