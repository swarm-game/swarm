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
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Terrain (TerrainMap, TerrainType)
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Game.World.Coords
import Swarm.TUI.Editor.Model
import Swarm.Util.Content
import Swarm.Util.Erasable

getEntitiesForList :: EntityMap -> V.Vector EntityFacade
getEntitiesForList em =
  V.fromList $ map mkFacade entities
 where
  entities = M.elems $ entitiesByName em

getEditingBounds :: WorldDescription -> (Bool, Cosmic BoundsRectangle)
getEditingBounds myWorld =
  (EA.isEmpty a, newBounds)
 where
  newBounds = Cosmic DefaultRootSubworld (locToCoords upperLeftLoc, locToCoords lowerRightLoc)
  upperLeftLoc = gridPosition $ area myWorld
  a = EA.getGridDimensions $ gridContent $ area myWorld
  lowerRightLoc = EA.computeBottomRightFromUpperLeft a upperLeftLoc

getEditorContentAt ::
  TerrainMap ->
  WorldOverdraw ->
  W.MultiWorld Int Entity ->
  Cosmic Coords ->
  (TerrainType, Maybe EntityPaint)
getEditorContentAt tm editorOverdraw w coords =
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
  (underlyingCellTerrain, underlyingCellEntity) = getContentAt tm w coords

getEditorTerrainAt ::
  TerrainMap ->
  WorldOverdraw ->
  W.MultiWorld Int Entity ->
  Cosmic Coords ->
  TerrainType
getEditorTerrainAt tm editor w coords =
  fst $ getEditorContentAt tm editor w coords

isOutsideTopLeftCorner ::
  -- | top left corner coords
  Coords ->
  -- | current coords
  Coords ->
  Bool
isOutsideTopLeftCorner (Coords (yTop, xLeft)) (Coords (y, x)) =
  x < xLeft || y < yTop

isOutsideBottomRightCorner ::
  -- | bottom right corner coords
  Coords ->
  -- | current coords
  Coords ->
  Bool
isOutsideBottomRightCorner (Coords (yBottom, xRight)) (Coords (y, x)) =
  x > xRight || y > yBottom

isOutsideRegion ::
  -- | full bounds
  BoundsRectangle ->
  -- | current coords
  Coords ->
  Bool
isOutsideRegion (tl, br) coord =
  isOutsideTopLeftCorner tl coord || isOutsideBottomRightCorner br coord

getEditedMapRectangle ::
  TerrainMap ->
  WorldOverdraw ->
  Maybe (Cosmic BoundsRectangle) ->
  W.MultiWorld Int Entity ->
  Grid CellPaintDisplay
getEditedMapRectangle _ _ Nothing _ = EmptyGrid
getEditedMapRectangle tm worldEditor (Just (Cosmic subworldName coords)) w =
  getMapRectangle toFacade getContent coords
 where
  getContent = getEditorContentAt tm worldEditor w . Cosmic subworldName
