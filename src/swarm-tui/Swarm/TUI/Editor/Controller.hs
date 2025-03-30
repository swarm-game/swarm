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

activateWorldEditorFunction :: Menu -> WorldEditorFocusable -> EventM Name PlayState ()
activateWorldEditorFunction m BrushSelector = openModal m TerrainPaletteModal
activateWorldEditorFunction m EntitySelector = openModal m EntityPaletteModal
activateWorldEditorFunction _ AreaSelector =
  Brick.zoom (uiGameplay . uiWorldEditor . editingBounds) $ do
    selectorStage <- use boundsSelectionStep
    case selectorStage of
      SelectionComplete -> boundsSelectionStep .= UpperLeftPending
      _ -> return ()
activateWorldEditorFunction _ OutputPathSelector =
  -- TODO: #1371
  liftIO $ putStrLn "File selection"
activateWorldEditorFunction _ MapSaveButton = saveMapFile
activateWorldEditorFunction _ ClearEntityButton =
  uiGameplay . uiWorldEditor . entityPaintList . BL.listSelectedL .= Nothing

handleCtrlLeftClick :: B.Location -> EventM Name PlayState ()
handleCtrlLeftClick mouseLoc = do
  worldEditor <- use $ uiGameplay . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. worldOverdraw . isWorldEditorEnabled
    let getSelected x = snd <$> BL.listSelectedElement x
        maybeTerrainType = getSelected $ worldEditor ^. terrainList
        maybeEntityPaint = getSelected $ worldEditor ^. entityPaintList
    terrain <- hoistMaybe maybeTerrainType
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    Brick.zoom (uiGameplay . uiWorldEditor) $ do
      worldOverdraw . paintedTerrain %= M.insert (mouseCoords ^. planar) (terrain, maybeToErasable maybeEntityPaint)
      lastWorldEditorMessage .= Nothing
  Brick.zoom gameState immediatelyRedrawWorld

handleRightClick :: B.Location -> EventM Name PlayState ()
handleRightClick mouseLoc = do
  worldEditor <- use $ uiGameplay . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. worldOverdraw . isWorldEditorEnabled
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    uiGameplay . uiWorldEditor . worldOverdraw . paintedTerrain %= M.delete (mouseCoords ^. planar)
  Brick.zoom gameState immediatelyRedrawWorld

-- | "Eye Dropper" tool:
handleMiddleClick :: B.Location -> EventM Name PlayState ()
handleMiddleClick mouseLoc = do
  worldEditor <- use $ uiGameplay . uiWorldEditor
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
          uiGameplay . uiWorldEditor . terrainList %= BL.listMoveToElement terrain
          forM_ maybeElementPaint $ \elementPaint ->
            let p = case elementPaint of
                  Facade efd -> efd
                  Ref r -> mkFacade r
             in uiGameplay . uiWorldEditor . entityPaintList %= BL.listMoveToElement p

    mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    whenJust mouseCoordsM setTerrainPaint

-- | Handle user input events in the robot panel.
handleWorldEditorPanelEvent :: Menu -> BrickEvent Name AppEvent -> EventM Name PlayState ()
handleWorldEditorPanelEvent m = \case
  Key V.KEsc -> uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
  Key V.KEnter -> do
    fring <- use $ uiGameplay . uiWorldEditor . editorFocusRing
    case focusGetCurrent fring of
      Just (WorldEditorPanelControl x) -> activateWorldEditorFunction m x
      _ -> return ()
  ControlChar 's' -> saveMapFile
  CharKey '\t' -> uiGameplay . uiWorldEditor . editorFocusRing %= focusNext
  Key V.KBackTab -> uiGameplay . uiWorldEditor . editorFocusRing %= focusPrev
  _ -> return ()

-- | Return value: whether the cursor position should be updated
updateAreaBounds :: Maybe (Cosmic Coords) -> EventM Name PlayState Bool
updateAreaBounds = \case
  Nothing -> return True
  Just mouseCoords -> do
    selectorStage <- use $ uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep
    case selectorStage of
      UpperLeftPending -> do
        uiGameplay . uiWorldEditor . editingBounds . boundsSelectionStep .= LowerRightPending mouseCoords
        return False
      -- TODO (#1152): Validate that the lower-right click is below and to the right of
      -- the top-left coord and that they are within the same subworld
      LowerRightPending upperLeftMouseCoords -> do
        t <- liftIO $ getTime Monotonic
        Brick.zoom (uiGameplay . uiWorldEditor) $ do
          lastWorldEditorMessage .= Nothing
          Brick.zoom editingBounds $ do
            boundsRect .= Just (fmap (,view planar mouseCoords) upperLeftMouseCoords)
            boundsSelectionStep .= SelectionComplete
            boundsPersistDisplayUntil .= t + TimeSpec 2 0
        setFocus WorldEditorPanel
        return False
      SelectionComplete -> return True

saveMapFile :: EventM Name PlayState ()
saveMapFile = do
  uig <- use uiGameplay
  land <- use $ gameState . landscape
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

  uiGameplay . uiWorldEditor . lastWorldEditorMessage .= Just "Saved."
