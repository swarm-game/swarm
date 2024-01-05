-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for accessing content of the world,
-- by single cells or in bulk for rendering.
module Swarm.Util.Content where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Data.Map qualified as M
import Data.Text qualified as T
import Swarm.Game.Display
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Entity.Cosmetic.Assignment (terrainAttributes)
import Swarm.Game.Scenario.Topography.Area qualified as EA
import Swarm.Game.Scenario.Topography.Cell (PCell (..))
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Terrain (TerrainType, getTerrainWord)
import Swarm.Game.Universe
import Swarm.Game.World
import Swarm.Util.Erasable (erasableToMaybe, maybeToErasable)

-- | Get the terrain and entity at a single cell
getContentAt :: MultiWorld Int e -> Cosmic Coords -> (TerrainType, Maybe e)
getContentAt w coords = (underlyingCellTerrain, underlyingCellEntity)
 where
  underlyingCellEntity = lookupCosmicEntity coords w
  underlyingCellTerrain = lookupCosmicTerrain coords w

-- * Rendering

-- | Get a rectangle of cells for rendering
getMapRectangle ::
  (d -> e) ->
  (Coords -> (TerrainType, Maybe d)) ->
  BoundsRectangle ->
  EA.Grid (PCell e)
getMapRectangle paintTransform contentFunc coords =
  EA.Grid $ map renderRow [yTop .. yBottom]
 where
  (Coords (yTop, xLeft), Coords (yBottom, xRight)) = coords

  drawCell f rowIndex colIndex =
    Cell
      terrain
      (f <$> maybeToErasable erasableEntity)
      []
   where
    (terrain, erasableEntity) = contentFunc $ Coords (rowIndex, colIndex)

  renderRow rowIndex = map (drawCell paintTransform rowIndex) [xLeft .. xRight]

-- | Get the color used to render a single cell
getTerrainEntityColor ::
  M.Map WorldAttr PreservableColor ->
  PCell EntityFacade ->
  Maybe PreservableColor
getTerrainEntityColor aMap (Cell terr cellEnt _) =
  (entityColor =<< erasableToMaybe cellEnt) <|> terrainFallback
 where
  terrainFallback = M.lookup (TerrainAttr $ T.unpack $ getTerrainWord terr) terrainAttributes
  entityColor (EntityFacade _ d) = case d ^. displayAttr of
    AWorld n -> M.lookup (WorldAttr $ T.unpack n) aMap
    _ -> Nothing
