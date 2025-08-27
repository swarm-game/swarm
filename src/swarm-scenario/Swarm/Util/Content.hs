-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for accessing content of the world,
-- by single cells or in bulk for rendering.
module Swarm.Util.Content where

import Data.Map qualified as M
import Swarm.Game.Cosmetic.Attribute
import Swarm.Game.Cosmetic.Color (AttributeMap, PreservableColor)
import Swarm.Game.Cosmetic.Display (renderDisplay)
import Swarm.Game.Cosmetic.Texel (getTexelColor, texelFromColor)
import Swarm.Game.Scenario.Topography.Cell (PCell (..))
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Terrain (TerrainMap, TerrainType, getTerrainWord)
import Swarm.Game.Universe
import Swarm.Game.World
import Swarm.Util.Erasable (erasableToMaybe, maybeToErasable)

-- | Get the terrain and entity at a single cell
getContentAt :: TerrainMap -> MultiWorld Int e -> Cosmic Coords -> (TerrainType, Maybe e)
getContentAt tm w coords = (underlyingCellTerrain, underlyingCellEntity)
 where
  underlyingCellEntity = lookupCosmicEntity coords w
  underlyingCellTerrain = lookupCosmicTerrain tm coords w

-- * Rendering

-- | Get a rectangle of cells for rendering.
getMapRectangle ::
  (d -> e) ->
  (Coords -> (TerrainType, Maybe d)) ->
  BoundsRectangle ->
  Grid (PCell e)
getMapRectangle paintTransform contentFunc coords =
  mkGrid $ map renderRow [yTop .. yBottom]
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

-- | Get the color used to render a single cell, by combining the texels for the
--   entity and terrain, and extracting its color.
getTerrainEntityColor ::
  AttributeMap ->
  PCell EntityFacade ->
  Maybe PreservableColor
getTerrainEntityColor aMap (Cell terr cellEnt _) =
  getTexelColor $ entityTexel <> terrainTexel
 where
  entityTexel = maybe mempty renderEntityTexel (erasableToMaybe cellEnt)
  terrainTexel =
    maybe mempty (texelFromColor 0 ' ') $
      M.lookup (AWorld $ getTerrainWord terr) aMap
  renderEntityTexel (EntityFacade _ disp hdg) = renderDisplay aMap hdg (const False) True disp
