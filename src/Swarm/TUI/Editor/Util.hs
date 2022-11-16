module Swarm.TUI.Editor.Util where

import Control.Lens hiding (Const, from)
import Data.Char qualified as DC
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Swarm.Game.Entity (Entity)
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.World qualified as W
import Swarm.TUI.Model

getTerrainAt :: WorldEditor -> W.World Int Entity -> W.Coords -> TerrainType
getTerrainAt editor w coords = case editor ^. isWorldEditorEnabled of
  True -> Maybe.fromMaybe underlyingCell $ Map.lookup coords paintMap
  False -> underlyingCell
 where
  paintMap = editor ^. paintedTerrain
  underlyingCell = toEnum $ W.lookupTerrain coords w

getEditedMapAsString :: WorldEditor -> Maybe (W.Coords, W.Coords) -> W.World Int Entity -> String
getEditedMapAsString _ Nothing _ = "EMPTY BOUNDS"
getEditedMapAsString worldEditor (Just (W.Coords (xLeft, yTop), W.Coords (xRight, yBottom))) w =
  unlines $ map renderLine [yTop .. yBottom]
 where
  getTerrain = getTerrainAt worldEditor w
  drawCell :: Int64 -> Int64 -> Char
  drawCell rowIndex = DC.chr . (+ DC.ord '0') . fromEnum . getTerrain . W.Coords . (rowIndex,)
  renderLine rowIndex = map (drawCell rowIndex) [xLeft .. xRight]
