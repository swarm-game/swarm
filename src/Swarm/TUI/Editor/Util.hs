-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.Util where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from)
import Control.Monad (guard)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Vector qualified as V
import Swarm.Game.Entity
import Swarm.Game.Scenario.Topography.Area qualified as EA
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Model
import Swarm.Util.Content
import Swarm.Util.Erasable

getEntitiesForList :: EntityMap -> V.Vector EntityFacade
getEntitiesForList em =
  V.fromList $ map mkFacade entities
 where
  entities = M.elems $ entitiesByName em

getEditingBounds :: WorldDescription -> (Bool, Cosmic W.BoundsRectangle)
getEditingBounds myWorld =
  (EA.isEmpty a, newBounds)
 where
  newBounds = Cosmic DefaultRootSubworld (W.locToCoords upperLeftLoc, W.locToCoords lowerRightLoc)
  upperLeftLoc = ul myWorld
  a = EA.getAreaDimensions $ area myWorld
  lowerRightLoc = EA.upperLeftToBottomRight a upperLeftLoc

getEditorContentAt ::
  WorldOverdraw ->
  W.MultiWorld Int Entity ->
  Cosmic W.Coords ->
  (TerrainType, Maybe EntityPaint)
getEditorContentAt editorOverdraw w coords =
  (terrainWithOverride, entityWithOverride)
 where
  terrainWithOverride = Maybe.fromMaybe underlyingCellTerrain $ do
    (terrainOverride, _) <- maybePaintedCell
    return terrainOverride

  maybeEntityOverride :: Maybe EntityPaint
  maybeEntityOverride = do
    (_, e) <- maybePaintedCell
    Facade <$> erasableToMaybe e

  maybePaintedCell = do
    guard $ editorOverdraw ^. isWorldEditorEnabled
    Map.lookup (coords ^. planar) pm

  pm = editorOverdraw ^. paintedTerrain

  entityWithOverride = (Ref <$> underlyingCellEntity) <|> maybeEntityOverride
  (underlyingCellTerrain, underlyingCellEntity) = getContentAt w coords

getEditorTerrainAt ::
  WorldOverdraw ->
  W.MultiWorld Int Entity ->
  Cosmic W.Coords ->
  TerrainType
getEditorTerrainAt editor w coords =
  fst $ getEditorContentAt editor w coords

isOutsideTopLeftCorner ::
  -- | top left corner coords
  W.Coords ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideTopLeftCorner (W.Coords (yTop, xLeft)) (W.Coords (y, x)) =
  x < xLeft || y < yTop

isOutsideBottomRightCorner ::
  -- | bottom right corner coords
  W.Coords ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideBottomRightCorner (W.Coords (yBottom, xRight)) (W.Coords (y, x)) =
  x > xRight || y > yBottom

isOutsideRegion ::
  -- | full bounds
  W.BoundsRectangle ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideRegion (tl, br) coord =
  isOutsideTopLeftCorner tl coord || isOutsideBottomRightCorner br coord

getEditedMapRectangle ::
  WorldOverdraw ->
  Maybe (Cosmic W.BoundsRectangle) ->
  W.MultiWorld Int Entity ->
  EA.Grid CellPaintDisplay
getEditedMapRectangle _ Nothing _ = EA.Grid []
getEditedMapRectangle worldEditor (Just (Cosmic subworldName coords)) w =
  getMapRectangle toFacade getContent coords
 where
  getContent = getEditorContentAt worldEditor w . Cosmic subworldName
