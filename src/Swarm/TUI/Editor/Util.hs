module Swarm.TUI.Editor.Util where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, from)
import Control.Monad (guard)
import Data.Int (Int32)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Cell
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.EditorModel
import Swarm.TUI.Model

getContentAt ::
  WorldEditor Name ->
  W.World Int Entity ->
  W.Coords ->
  (TerrainType, Maybe EntityPaint)
getContentAt editor w coords =
  (terrainWithOverride, entityWithOverride)
 where
  terrainWithOverride = Maybe.fromMaybe underlyingCellTerrain $ do
    (terrainOverride, _) <- maybePaintedCell
    return terrainOverride

  maybeEntityOverride = do
    (_, e) <- maybePaintedCell
    Facade <$> e

  maybePaintedCell = do
    guard $ editor ^. isWorldEditorEnabled
    Map.lookup coords paintMap

  paintMap = editor ^. paintedTerrain

  entityWithOverride = (Ref <$> underlyingCellEntity) <|> maybeEntityOverride
  underlyingCellEntity = W.lookupEntity coords w
  underlyingCellTerrain = toEnum $ W.lookupTerrain coords w

getTerrainAt ::
  WorldEditor Name ->
  W.World Int Entity ->
  W.Coords ->
  TerrainType
getTerrainAt editor w coords = fst $ getContentAt editor w coords

isOutsideTopLeftCorner ::
  -- | corner coords
  W.Coords ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideTopLeftCorner (W.Coords (xLeft, yTop)) (W.Coords (x, y)) =
  x < xLeft || y < yTop

isOutsideRegion ::
  -- | full bounds
  (W.Coords, W.Coords) ->
  -- | current coords
  W.Coords ->
  Bool
isOutsideRegion (tl, W.Coords (xRight, yBottom)) c@(W.Coords (x, y)) =
  isOutsideTopLeftCorner tl c
    || (x > xRight || y > yBottom)

getEditedMapRectangle ::
  WorldEditor Name ->
  Maybe (W.Coords, W.Coords) ->
  W.World Int Entity ->
  [[CellPaintDisplay]]
getEditedMapRectangle _ Nothing _ = []
getEditedMapRectangle worldEditor (Just (tl, br)) w =
  map renderRow [yTop .. yBottom]
 where
  -- We swap the horizontal and vertical coordinate.
  -- TODO If this is necessary, then what is mouseLocToWorldCoords for?
  toWorldCoords (W.Coords (mx, my)) = W.Coords (my, mx)

  W.Coords (xLeft, yTop) = toWorldCoords tl
  W.Coords (xRight, yBottom) = toWorldCoords br

  getContent = getContentAt worldEditor w

  drawCell :: Int32 -> Int32 -> CellPaintDisplay
  drawCell rowIndex colIndex =
    Cell
      terrain
      (toFacade <$> maybeEntity)
      []
   where
    (terrain, maybeEntity) = getContent $ W.Coords (rowIndex, colIndex)

  renderRow rowIndex = map (drawCell rowIndex) [xLeft .. xRight]
