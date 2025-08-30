{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Pure world definition and functions.
--
-- A /world/ refers to the grid on which the game takes place, and the
-- things in it (besides robots). A world has a base, immutable
-- /terrain/ layer, where each cell contains a terrain type, and a
-- mutable /entity/ layer, with at most one entity per cell.
--
-- A world is technically finite but practically infinite (worlds are
-- indexed by 32-bit signed integers, so they correspond to a
-- \( 2^{32} \times 2^{32} \) torus).
--
-- Prefer using the stateful versions for lookup in 'Swarm.Game.World.Stateful',
-- which internally cache the loaded regions.
module Swarm.Game.World.Pure (
  World (..),
  newWorld,

  -- ** Lookup
  lookupTerrain,
  lookupEntity,

  -- ** Update
  update,

  -- ** Loading
  loadCell,
  loadRegion,
) where

import Control.Arrow (second)
import Control.Lens hiding (use)
import Data.Array qualified as A
import Data.Array.IArray
import Data.Array.Unboxed qualified as U
import Data.Foldable (foldl')
import Data.Map.Strict qualified as M
import Data.Strict qualified as Strict
import Swarm.Game.Entity (Entity, entityHash)
import Swarm.Game.Scenario.Topography.Modify
import Swarm.Game.World.Coords
import Swarm.Game.World.Function
import Swarm.Game.World.Tile
import Swarm.Util ((?))
import Prelude hiding (Foldable (..), lookup)

-- | A 'World' consists of a 'WorldFun' that specifies the initial
--   world, a cache of loaded square tiles to make lookups faster, and
--   a map storing locations whose entities have changed from their
--   initial values.
--
--   Right now the 'World' simply holds on to all the tiles it has
--   ever loaded.  Ideally it would use some kind of LRU caching
--   scheme to keep memory usage bounded, but it would be a bit
--   tricky, and in any case it's probably not going to matter much
--   for a while.  Once tile loads can trigger robots to spawn, it
--   would also make for some difficult decisions in terms of how to
--   handle respawning.
data World t e = World
  { worldFun :: WorldFun t e
  , tileCache :: M.Map TileCoords (Strict.Pair (TerrainTile t) (EntityTile e))
  , changed :: M.Map Coords (Strict.Maybe e)
  }

-- | Create a new 'World' from a 'WorldFun'.
newWorld :: WorldFun t e -> World t e
newWorld f = World f M.empty M.empty

-- | Look up the terrain value at certain coordinates: try looking it
--   up in the tile cache first, and fall back to running the 'WorldFun'
--   otherwise.
--
--   This function does /not/ ensure that the tile containing the
--   given coordinates is loaded.  For that, see 'lookupTerrainM'.
lookupTerrain :: (IArray U.UArray t) => Coords -> World t e -> t
lookupTerrain i (World f t _) =
  ((U.! tileOffset i) . Strict.fst <$> M.lookup (tileCoords i) t)
    ? fst (runWF f i)

-- | Look up the entity at certain coordinates: first, see if it is in
--   the map of locations with changed entities; then try looking it
--   up in the tile cache first; and finally fall back to running the
--   'WorldFun'.
--
--   This function does /not/ ensure that the tile containing the
--   given coordinates is loaded.  For that, see 'lookupEntityM'.
lookupEntity :: Coords -> World t e -> Maybe e
lookupEntity i (World f t m) = modifiedEntity ? cachedTileEntity ? computedEntity
 where
  modifiedEntity = toLazyMaybe <$> M.lookup i m
  cachedTileEntity = toLazyMaybe . (A.! tileOffset i) . Strict.snd <$> M.lookup (tileCoords i) t
  computedEntity = snd (runWF f i)

-- | Update the entity (or absence thereof) at a certain location,
--   returning an updated 'World' and a Boolean indicating whether
--   the update changed the entity here.
--   See also 'updateM'.
update ::
  Coords ->
  (Maybe Entity -> Maybe Entity) ->
  World t Entity ->
  (World t Entity, CellUpdate Entity)
update i g w@(World f t m) =
  (wNew, classifyModification (view entityHash) entityBefore entityAfter)
 where
  wNew = World f t $ M.insert i (toStrictMaybe entityAfter) m
  entityBefore = lookupEntity i w
  entityAfter = g entityBefore

-- | Load the tile containing a specific cell.
loadCell ::
  (IArray U.UArray t) =>
  Coords ->
  World t e ->
  World t e
loadCell c = fst . loadRegion (c, c)

-- | Load all the tiles which overlap the given rectangular region
--   (specified as an upper-left and lower-right corner, inclusive).
loadRegion ::
  forall t e.
  (IArray U.UArray t) =>
  (Coords, Coords) ->
  World t e ->
  (World t e, [TileCoords])
loadRegion reg (World f t m) = (World f t' m, tileCs)
 where
  -- the range is applied to tile coordinates, so we are not loading a tile twice
  tileCs = filter (`M.notMember` t) $ range (over both tileCoords reg)
  tiles = map loadTile tileCs
  t' = foldl' (\hm (i, tile) -> M.insert i tile hm) t (zip tileCs tiles)

  loadTile :: TileCoords -> Strict.Pair (TerrainTile t) (EntityTile e)
  loadTile tc = (listArray tileBounds terrain Strict.:!: listArray tileBounds entities)
   where
    tileCorner = tileOrigin tc
    runWF' = second toStrictMaybe . runWF f
    (terrain, entities) = unzip $ map (runWF' . plusOffset tileCorner) (range tileBounds)

toLazyMaybe :: Strict.Maybe a -> Maybe a
toLazyMaybe = Strict.maybe Nothing Just

toStrictMaybe :: Maybe a -> Strict.Maybe a
toStrictMaybe = maybe Strict.Nothing Strict.Just
