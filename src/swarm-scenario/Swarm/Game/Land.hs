{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Terrain and Entities
module Swarm.Game.Land (
  TerrainEntityMaps (TerrainEntityMaps),
  terrainMap,
  entityMap,
) where

import Control.Lens (makeLenses)
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Terrain

data TerrainEntityMaps = TerrainEntityMaps
  { _terrainMap :: TerrainMap
  , _entityMap :: EntityMap
  }
  deriving (Show, Generic)

makeLenses ''TerrainEntityMaps

instance Semigroup TerrainEntityMaps where
  TerrainEntityMaps tm1 em1 <> TerrainEntityMaps tm2 em2 =
    TerrainEntityMaps (tm1 <> tm2) (em1 <> em2)

instance Monoid TerrainEntityMaps where
  mempty = TerrainEntityMaps mempty mempty
