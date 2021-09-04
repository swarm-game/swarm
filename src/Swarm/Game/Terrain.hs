{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Terrain
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Terrain types and properties.
--
-----------------------------------------------------------------------------

module Swarm.Game.Terrain
  ( -- * Terrain

    TerrainType(..)
  , displayTerrain
  , terrainMap

  ) where

import           Brick              (Widget)
import           Data.Map           (Map, (!))
import qualified Data.Map           as M

import           Swarm.Game.Display
import           Swarm.TUI.Attr

-- | The different possible types of terrain. Unlike entities and
--   robots, these are hard-coded into the game.
data TerrainType
  = StoneT
  | DirtT
  | GrassT
  | WaterT
  | IceT
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Display a terrain type as a single charcter widget.
displayTerrain :: TerrainType -> Widget n
displayTerrain t = displayWidget Nothing (terrainMap ! t)

-- | A map containing a 'Display' record for each different 'TerrainType'.
terrainMap :: Map TerrainType Display
terrainMap = M.fromList
  [ (StoneT, defaultTerrainDisplay '░' rockAttr)
  , (DirtT, defaultTerrainDisplay '░' dirtAttr)
  , (GrassT, defaultTerrainDisplay '░' grassAttr)
  , (WaterT, defaultTerrainDisplay ' ' waterAttr)
  , (IceT, defaultTerrainDisplay ' ' iceAttr)
  ]
