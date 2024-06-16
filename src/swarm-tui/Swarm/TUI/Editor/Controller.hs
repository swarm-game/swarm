{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.Controller where

import Brick hiding (Direction (..), Location (..))
import Brick qualified as B
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens
import Control.Monad (forM_, guard, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Map qualified as M
import Data.Yaml qualified as Y
import Graphics.Vty qualified as V
import Swarm.Game.Land
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Palette
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import Swarm.Util (hoistMaybe)
import Swarm.Util.Erasable (maybeToErasable)
import System.Clock

------------------------------------------------------------
-- World Editor panel events
------------------------------------------------------------

activateWorldEditorFunction :: WorldEditorFocusable -> EventM Name AppState ()
activateWorldEditorFunction BrushSelector = openModal TerrainPaletteModal
activateWorldEditorFunction EntitySelector = openModal EntityPaletteModal
activateWorldEditorFunction AreaSelector = do
  selectorStage <- use $ uiState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep
  case selectorStage of
    SelectionComplete -> uiState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= UpperLeftPending
    _ -> return ()
activateWorldEditorFunction OutputPathSelector =
  -- TODO: #1371
  liftIO $ putStrLn "File selection"
activateWorldEditorFunction MapSaveButton = saveMapFile
activateWorldEditorFunction ClearEntityButton =
  uiState . uiGameplay . uiWorldEditor . entityPaintList . BL.listSelectedL .= Nothing

handleCtrlLeftClick :: B.Location -> EventM Name AppState ()
handleCtrlLeftClick mouseLoc = do
  worldEditor <- use $ uiState . uiGameplay . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. worldOverdraw . isWorldEditorEnabled
    let getSelected x = snd <$> BL.listSelectedElement x
        maybeTerrainType = getSelected $ worldEditor ^. terrainList
        maybeEntityPaint = getSelected $ worldEditor ^. entityPaintList
    terrain <- hoistMaybe maybeTerrainType
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    Brick.zoom (uiState . uiGameplay . uiWorldEditor) $ do
      worldOverdraw . paintedTerrain %= M.insert (mouseCoords ^. planar) (terrain, maybeToErasable maybeEntityPaint)
      lastWorldEditorMessage .= Nothing
  immediatelyRedrawWorld
  return ()

handleRightClick :: B.Location -> EventM Name AppState ()
handleRightClick mouseLoc = do
  worldEditor <- use $ uiState . uiGameplay . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. worldOverdraw . isWorldEditorEnabled
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    uiState . uiGameplay . uiWorldEditor . worldOverdraw . paintedTerrain %= M.delete (mouseCoords ^. planar)
  immediatelyRedrawWorld
  return ()

-- | "Eye Dropper" tool:
handleMiddleClick :: B.Location -> EventM Name AppState ()
handleMiddleClick mouseLoc = do
  worldEditor <- use $ uiState . uiGameplay . uiWorldEditor
  when (worldEditor ^. worldOverdraw . isWorldEditorEnabled) $ do
    w <- use $ gameState . landscape . multiWorld
    tm <- use $ gameState . landscape . terrainAndEntities . terrainMap
    let setTerrainPaint coords = do
          let (terrain, maybeElementPaint) =
                EU.getEditorContentAt
                  tm
                  (worldEditor ^. worldOverdraw)
                  w
                  coords
          uiState . uiGameplay . uiWorldEditor . terrainList %= BL.listMoveToElement terrain
          forM_ maybeElementPaint $ \elementPaint ->
            let p = case elementPaint of
                  Facade efd -> efd
                  Ref r -> mkFacade r
             in uiState . uiGameplay . uiWorldEditor . entityPaintList %= BL.listMoveToElement p

    mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    whenJust mouseCoordsM setTerrainPaint

-- | Handle user input events in the robot panel.
handleWorldEditorPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWorldEditorPanelEvent = \case
  Key V.KEsc -> uiState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
  Key V.KEnter -> do
    fring <- use $ uiState . uiGameplay . uiWorldEditor . editorFocusRing
    case focusGetCurrent fring of
      Just (WorldEditorPanelControl x) -> activateWorldEditorFunction x
      _ -> return ()
  ControlChar 's' -> saveMapFile
  CharKey '\t' -> uiState . uiGameplay . uiWorldEditor . editorFocusRing %= focusNext
  Key V.KBackTab -> uiState . uiGameplay . uiWorldEditor . editorFocusRing %= focusPrev
  _ -> return ()

-- | Return value: whether the cursor position should be updated
updateAreaBounds :: Maybe (Cosmic Coords) -> EventM Name AppState Bool
updateAreaBounds = \case
  Nothing -> return True
  Just mouseCoords -> do
    selectorStage <- use $ uiState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep
    case selectorStage of
      UpperLeftPending -> do
        uiState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= LowerRightPending mouseCoords
        return False
      -- TODO (#1152): Validate that the lower-right click is below and to the right of
      -- the top-left coord and that they are within the same subworld
      LowerRightPending upperLeftMouseCoords -> do
        t <- liftIO $ getTime Monotonic
        Brick.zoom (uiState . uiGameplay . uiWorldEditor) $ do
          lastWorldEditorMessage .= Nothing
          Brick.zoom editingBounds $ do
            boundsRect .= Just (fmap (,view planar mouseCoords) upperLeftMouseCoords)
            boundsSelectionStep .= SelectionComplete
            boundsPersistDisplayUntil .= t + TimeSpec 2 0
        setFocus WorldEditorPanel
        return False
      SelectionComplete -> return True

saveMapFile :: EventM Name AppState ()
saveMapFile = do
  worldEditor <- use $ uiState . uiGameplay . uiWorldEditor
  maybeBounds <- use $ uiState . uiGameplay . uiWorldEditor . editingBounds . boundsRect
  w <- use $ gameState . landscape . multiWorld
  tm <- use $ gameState . landscape . terrainAndEntities . terrainMap
  let mapCellGrid =
        mapRows (map (map Just)) $
          EU.getEditedMapRectangle tm (worldEditor ^. worldOverdraw) maybeBounds w

  let fp = worldEditor ^. outputFilePath
  maybeScenarioPair <- use $ uiState . uiGameplay . scenarioRef
  liftIO $ Y.encodeFile fp $ constructScenario (fst <$> maybeScenarioPair) mapCellGrid

  uiState . uiGameplay . uiWorldEditor . lastWorldEditorMessage .= Just "Saved."
