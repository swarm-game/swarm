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
import Swarm.TUI.Model.UI.Gameplay
import Swarm.Util (hoistMaybe)
import Swarm.Util.Erasable (maybeToErasable)
import System.Clock

------------------------------------------------------------
-- World Editor panel events
------------------------------------------------------------

activateWorldEditorFunction :: WorldEditorFocusable -> EventM Name AppState ()
activateWorldEditorFunction BrushSelector = openModal TerrainPaletteModal
activateWorldEditorFunction EntitySelector = openModal EntityPaletteModal
activateWorldEditorFunction AreaSelector =
  Brick.zoom (playState . uiGameplay . uiWorldEditor . editingBounds) $ do
    selectorStage <- use boundsSelectionStep
    case selectorStage of
      SelectionComplete -> boundsSelectionStep .= UpperLeftPending
      _ -> return ()
activateWorldEditorFunction OutputPathSelector =
  -- TODO: #1371
  liftIO $ putStrLn "File selection"
activateWorldEditorFunction MapSaveButton = saveMapFile
activateWorldEditorFunction ClearEntityButton =
  playState . uiGameplay . uiWorldEditor . entityPaintList . BL.listSelectedL .= Nothing

handleCtrlLeftClick :: B.Location -> EventM Name AppState ()
handleCtrlLeftClick mouseLoc = do
  worldEditor <- use $ playState . uiGameplay . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. worldOverdraw . isWorldEditorEnabled
    let getSelected x = snd <$> BL.listSelectedElement x
        maybeTerrainType = getSelected $ worldEditor ^. terrainList
        maybeEntityPaint = getSelected $ worldEditor ^. entityPaintList
    terrain <- hoistMaybe maybeTerrainType
    mouseCoords <- MaybeT $ Brick.zoom (playState . gameState) $ mouseLocToWorldCoords mouseLoc
    Brick.zoom (playState . uiGameplay . uiWorldEditor) $ do
      worldOverdraw . paintedTerrain %= M.insert (mouseCoords ^. planar) (terrain, maybeToErasable maybeEntityPaint)
      lastWorldEditorMessage .= Nothing
  immediatelyRedrawWorld

handleRightClick :: B.Location -> EventM Name AppState ()
handleRightClick mouseLoc = do
  worldEditor <- use $ playState . uiGameplay . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. worldOverdraw . isWorldEditorEnabled
    mouseCoords <- MaybeT $ Brick.zoom (playState . gameState) $ mouseLocToWorldCoords mouseLoc
    playState . uiGameplay . uiWorldEditor . worldOverdraw . paintedTerrain %= M.delete (mouseCoords ^. planar)
  immediatelyRedrawWorld

-- | "Eye Dropper" tool:
handleMiddleClick :: B.Location -> EventM Name AppState ()
handleMiddleClick mouseLoc = do
  worldEditor <- use $ playState . uiGameplay . uiWorldEditor
  when (worldEditor ^. worldOverdraw . isWorldEditorEnabled) $ do
    w <- use $ playState . gameState . landscape . multiWorld
    tm <- use $ playState . gameState . landscape . terrainAndEntities . terrainMap
    let setTerrainPaint coords = do
          let (terrain, maybeElementPaint) =
                EU.getEditorContentAt
                  tm
                  (worldEditor ^. worldOverdraw)
                  w
                  coords
          playState . uiGameplay . uiWorldEditor . terrainList %= BL.listMoveToElement terrain
          forM_ maybeElementPaint $ \elementPaint ->
            let p = case elementPaint of
                  Facade efd -> efd
                  Ref r -> mkFacade r
             in playState . uiGameplay . uiWorldEditor . entityPaintList %= BL.listMoveToElement p

    mouseCoordsM <- Brick.zoom (playState . gameState) $ mouseLocToWorldCoords mouseLoc
    whenJust mouseCoordsM setTerrainPaint

-- | Handle user input events in the robot panel.
handleWorldEditorPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWorldEditorPanelEvent = \case
  Key V.KEsc -> playState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
  Key V.KEnter -> do
    fring <- use $ playState . uiGameplay . uiWorldEditor . editorFocusRing
    case focusGetCurrent fring of
      Just (WorldEditorPanelControl x) -> activateWorldEditorFunction x
      _ -> return ()
  ControlChar 's' -> saveMapFile
  CharKey '\t' -> playState . uiGameplay . uiWorldEditor . editorFocusRing %= focusNext
  Key V.KBackTab -> playState . uiGameplay . uiWorldEditor . editorFocusRing %= focusPrev
  _ -> return ()

-- | Return value: whether the cursor position should be updated
updateAreaBounds :: Maybe (Cosmic Coords) -> EventM Name AppState Bool
updateAreaBounds = \case
  Nothing -> return True
  Just mouseCoords -> do
    selectorStage <- use $ playState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep
    case selectorStage of
      UpperLeftPending -> do
        playState . uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= LowerRightPending mouseCoords
        return False
      -- TODO (#1152): Validate that the lower-right click is below and to the right of
      -- the top-left coord and that they are within the same subworld
      LowerRightPending upperLeftMouseCoords -> do
        t <- liftIO $ getTime Monotonic
        Brick.zoom (playState . uiGameplay . uiWorldEditor) $ do
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
  uig <- use $ playState . uiGameplay
  land <- use $ playState . gameState . landscape
  let worldEditor = uig ^. uiWorldEditor
      maybeBounds = uig ^. uiWorldEditor . editingBounds . boundsRect

      w = land ^. multiWorld
      tm = land ^. terrainAndEntities . terrainMap
      mapCellGrid =
        Just
          <$> EU.getEditedMapRectangle tm (worldEditor ^. worldOverdraw) maybeBounds w

      fp = worldEditor ^. outputFilePath
      maybeScenarioPair = uig ^. scenarioRef

  liftIO $ Y.encodeFile fp $ constructScenario (fst <$> maybeScenarioPair) mapCellGrid

  playState . uiGameplay . uiWorldEditor . lastWorldEditorMessage .= Just "Saved."
