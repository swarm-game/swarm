{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Grid on which the game takes place
--
-- A /world/ refers to the grid on which the game takes place, and the
-- things in it (besides robots). A world has a base, immutable
-- /terrain/ layer, where each cell contains a terrain type, and a
-- mutable /entity/ layer, with at most one entity per cell.
--
-- A world is technically finite but practically infinite (worlds are
-- indexed by 32-bit signed integers, so they correspond to a
-- \( 2^{32} \times 2^{32} \) torus).
module Swarm.Game.World (
  -- * Worlds
  WorldFun (..),
  runWF,
  worldFunFromArray,
  World,
  MultiWorld,

  -- ** Tile management
  loadCell,
  loadRegion,

  -- ** World functions
  newWorld,
  lookupCosmicTerrain,
  lookupTerrain,
  lookupCosmicEntity,
  lookupEntity,
  update,

  -- ** Monadic variants
  lookupTerrainM,
  lookupEntityM,
  lookupContentM,
  updateM,
  loadRegionM,

  -- ** Runtime updates
  WorldUpdate (..),
) where

import Data.IntMap qualified as IM
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Swarm.Game.Location
import Swarm.Game.Terrain (TerrainMap, TerrainType (BlankT), terrainByIndex, terrainName)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Game.World.Function
import Swarm.Game.World.Pure
import Swarm.Game.World.Stateful
import Prelude hiding (Foldable (..), lookup)

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

data WorldUpdate e = ReplaceEntity
  { updatedLoc :: Cosmic Location
  , originalEntity :: e
  , newEntity :: Maybe e
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
