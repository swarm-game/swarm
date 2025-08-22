{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- World update result record.
module Swarm.Game.World.Update (
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

data WorldUpdate e = ReplaceEntity
  { updatedLoc :: Cosmic Location
  , originalEntity :: e
  , newEntity :: Maybe e
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
