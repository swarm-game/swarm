{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Control.Algebra (Has)
import Control.Effect.Lens (use)
import Control.Effect.State (State, get, modify, state)
import Control.Lens hiding (use)
import Control.Parallel.Strategies (evalTuple2, parMap, rpar, rseq)
import Data.Array qualified as A
import Data.Array.IArray
import Data.Array.Unboxed qualified as U
import Data.Foldable (foldl')
import Data.IntMap qualified as IM
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Swarm.Effect as Effect
import Swarm.Game.Entity (Entity, entityHash)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Modify
import Swarm.Game.Terrain (TerrainMap, TerrainType (BlankT), terrainByIndex, terrainName)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Game.World.Function
import Swarm.Game.World.Metrics (WorldMetrics (..))
import Swarm.Game.World.Tile
import Swarm.Util ((?))
import Prelude hiding (Foldable (..), lookup)

type MultiWorld t e = Map SubworldName (World t e)

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
  { _worldFun :: WorldFun t e
  , _tileCache :: M.Map TileCoords (TerrainTile t, EntityTile e)
  , _changed :: M.Map Coords (Maybe e)
  }

makeLenses 'World

-- | Create a new 'World' from a 'WorldFun'.
newWorld :: WorldFun t e -> World t e
newWorld f = World f M.empty M.empty

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

-- | Look up the terrain value at certain coordinates: try looking it
--   up in the tile cache first, and fall back to running the 'WorldFun'
--   otherwise.
--
--   This function does /not/ ensure that the tile containing the
--   given coordinates is loaded.  For that, see 'lookupTerrainM'.
lookupTerrain :: (IArray U.UArray t) => Coords -> World t e -> t
lookupTerrain i (World f t _) =
  ((U.! tileOffset i) . fst <$> M.lookup (tileCoords i) t)
    ? fst (runWF f i)

-- | A stateful variant of 'lookupTerrain', which first loads the tile
--   containing the given coordinates if it is not already loaded,
--   then looks up the terrain value.
lookupTerrainM ::
  forall t e sig m.
  (Has (State (World t e)) sig m, IArray U.UArray t) =>
  Coords ->
  m t
lookupTerrainM c = do
  modify @(World t e) $ loadCell c
  lookupTerrain c <$> get @(World t e)

lookupContentM ::
  forall t e sig m.
  (Has (State (World t e)) sig m, IArray U.UArray t) =>
  Coords ->
  m (t, Maybe e)
lookupContentM c = do
  modify @(World t e) $ loadCell c
  w <- get @(World t e)
  return (lookupTerrain c w, lookupEntity c w)

lookupCosmicEntity :: Cosmic Coords -> MultiWorld t e -> Maybe e
lookupCosmicEntity (Cosmic subworldName i) multiWorld =
  lookupEntity i =<< M.lookup subworldName multiWorld

-- | Look up the entity at certain coordinates: first, see if it is in
--   the map of locations with changed entities; then try looking it
--   up in the tile cache first; and finally fall back to running the
--   'WorldFun'.
--
--   This function does /not/ ensure that the tile containing the
--   given coordinates is loaded.  For that, see 'lookupEntityM'.
lookupEntity :: Coords -> World t e -> Maybe e
lookupEntity i (World f t m) =
  M.lookup i m
    ? ((A.! tileOffset i) . snd <$> M.lookup (tileCoords i) t)
    ? snd (runWF f i)

-- | A stateful variant of 'lookupEntity', which first loads the tile
--   containing the given coordinates if it is not already loaded,
--   then looks up the terrain value.
lookupEntityM ::
  forall t e sig m.
  (Has (State (World t e)) sig m, IArray U.UArray t) =>
  Coords ->
  m (Maybe e)
lookupEntityM c = do
  modify @(World t e) $ loadCell c
  lookupEntity c <$> get @(World t e)

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
  wNew = World f t $ M.insert i entityAfter m
  entityBefore = lookupEntity i w
  entityAfter = g entityBefore

-- | A stateful variant of 'update', which also ensures the tile
--   containing the given coordinates is loaded.
updateM ::
  forall t sig m.
  (Has (State (World t Entity)) sig m, IArray U.UArray t) =>
  Coords ->
  (Maybe Entity -> Maybe Entity) ->
  m (CellUpdate Entity)
updateM c g = do
  state @(World t Entity) $ update c g . loadCell c

-- | Load the tile containing a specific cell.
loadCell :: (IArray U.UArray t) => Coords -> World t e -> World t e
loadCell c = loadRegion (c, c)

loadRegionM ::
  forall t e sig m.
  (IArray U.UArray t, Has (State (World t e)) sig m, Has Effect.Metric sig m, Has Effect.Time sig m) =>
  Maybe WorldMetrics ->
  (Coords, Coords) ->
  m ()
loadRegionM wm = updateMetric . modify @(World t e) . loadRegion
 where
  updateMetric m = case wm of
    Nothing -> m
    Just wMetrics -> do
      (loadTime, ()) <- Effect.measureCpuTimeInSec m
      tileCount <- use @(World t e) $ tileCache . to M.size
      Effect.gaugeSet wMetrics.loadedTiles tileCount
      Effect.distributionAdd wMetrics.tileLoadTime loadTime

-- | Load all the tiles which overlap the given rectangular region
--   (specified as an upper-left and lower-right corner, inclusive).
loadRegion ::
  forall t e.
  (IArray U.UArray t) =>
  (Coords, Coords) ->
  World t e ->
  World t e
loadRegion reg (World f t m) = World f t' m
 where
  -- the range is applied to tile coordinates, so we are not loading a tile twice
  tileCs = filter (`M.notMember` t) $ range (over both tileCoords reg)
  tiles = parMap (evalTuple2 rseq rpar) loadTile tileCs
  t' = foldl' (\hm (i, tile) -> M.insert i tile hm) t (zip tileCs tiles)

  loadTile :: TileCoords -> (TerrainTile t, EntityTile e)
  loadTile tc = (listArray tileBounds terrain, listArray tileBounds entities)
   where
    tileCorner = tileOrigin tc
    (terrain, entities) = unzip $ map (runWF f . plusOffset tileCorner) (range tileBounds)

---------------------------------------------------------------------
-- Runtime world update
---------------------------------------------------------------------

-- | Enumeration of world updates.  This type is used for changes by
--   /e.g./ the @drill@ command which must be carried out at a later
--   tick. Using a first-order representation (as opposed to /e.g./
--   just a @World -> World@ function) allows us to serialize and
--   inspect the updates.
data WorldUpdate e = ReplaceEntity
  { updatedLoc :: Cosmic Location
  , originalEntity :: e
  , newEntity :: Maybe e
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
