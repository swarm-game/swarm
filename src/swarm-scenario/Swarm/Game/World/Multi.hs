{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Named worlds and related functions.
module Swarm.Game.World.Multi (
  MultiWorld,

  -- ** Lookup
  lookupCosmicTerrain,
  lookupCosmicEntity,
) where

import Data.IntMap qualified as IM
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Swarm.Game.Terrain (TerrainMap, TerrainType (BlankT), terrainByIndex, terrainName)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Game.World.Pure

type MultiWorld t e = Map SubworldName (World t e)

lookupCosmicTerrain ::
  TerrainMap ->
  Cosmic Coords ->
  MultiWorld Int e ->
  TerrainType
lookupCosmicTerrain tm (Cosmic subworldName i) multiWorld =
  fromMaybe BlankT $ do
    x <- M.lookup subworldName multiWorld
    y <- (`IM.lookup` terrainByIndex tm) . lookupTerrain i $ x
    return $ terrainName y

lookupCosmicEntity :: Cosmic Coords -> MultiWorld t e -> Maybe e
lookupCosmicEntity (Cosmic subworldName i) multiWorld =
  lookupEntity i =<< M.lookup subworldName multiWorld
