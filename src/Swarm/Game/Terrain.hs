{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Swarm.Game.Terrain
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Terrain types and properties.
module Swarm.Game.Terrain (
  -- * Terrain
  TerrainType (..),
  displayTerrain,
  terrainMap,
) where

import Brick (Widget)
import Data.Aeson (FromJSON (..), withText)
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Read (readMaybe)
import Witch (into)

import Swarm.Game.Display
import Swarm.TUI.Attr

-- | The different possible types of terrain. Unlike entities and
--   robots, these are hard-coded into the game.
data TerrainType
  = StoneT
  | DirtT
  | GrassT
  | IceT
  | BlankT
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance FromJSON TerrainType where
  parseJSON = withText "text" $ \t ->
    case readMaybe (into @String (T.toTitle t) ++ "T") of
      Just ter -> return ter
      Nothing -> fail $ "Unknown terrain type: " ++ into @String t

-- | Display a terrain type as a single charcter widget.
displayTerrain :: TerrainType -> Widget n
displayTerrain t = displayWidget Nothing (terrainMap ! t)

-- | A map containing a 'Display' record for each different 'TerrainType'.
terrainMap :: Map TerrainType Display
terrainMap =
  M.fromList
    [ (StoneT, defaultTerrainDisplay '░' rockAttr)
    , (DirtT, defaultTerrainDisplay '░' dirtAttr)
    , (GrassT, defaultTerrainDisplay '░' grassAttr)
    , (IceT, defaultTerrainDisplay ' ' iceAttr)
    , (BlankT, defaultTerrainDisplay ' ' defAttr)
    ]
