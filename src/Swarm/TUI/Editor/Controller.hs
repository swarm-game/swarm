{-# LANGUAGE OverloadedStrings #-}

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
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Palette
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import System.Clock

------------------------------------------------------------
-- World Editor panel events
------------------------------------------------------------

activateWorldEditorFunction :: WorldEditorFocusable -> EventM Name AppState ()
activateWorldEditorFunction BrushSelector = openModal TerrainPaletteModal
activateWorldEditorFunction EntitySelector = openModal EntityPaletteModal
activateWorldEditorFunction AreaSelector = do
  selectorStage <- use $ uiState . uiWorldEditor . editingBounds . boundsSelectionStep
  case selectorStage of
    SelectionComplete -> uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= UpperLeftPending
    _ -> return ()
activateWorldEditorFunction OutputPathSelector =
  -- TODO: #1371
  liftIO $ putStrLn "File selection"
activateWorldEditorFunction MapSaveButton = saveMapFile
activateWorldEditorFunction ClearEntityButton =
  uiState . uiWorldEditor . entityPaintList . BL.listSelectedL .= Nothing

handleCtrlLeftClick :: B.Location -> EventM Name AppState ()
handleCtrlLeftClick mouseLoc = do
  worldEditor <- use $ uiState . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. isWorldEditorEnabled
    let getSelected x = snd <$> BL.listSelectedElement x
        maybeTerrainType = getSelected $ worldEditor ^. terrainList
        maybeEntityPaint = getSelected $ worldEditor ^. entityPaintList
    -- TODO (#1151): Use hoistMaybe when available
    terrain <- MaybeT . pure $ maybeTerrainType
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    uiState . uiWorldEditor . paintedTerrain %= M.insert (mouseCoords ^. planar) (terrain, maybeEntityPaint)
    uiState . uiWorldEditor . lastWorldEditorMessage .= Nothing
  immediatelyRedrawWorld
  return ()

handleRightClick :: B.Location -> EventM Name AppState ()
handleRightClick mouseLoc = do
  worldEditor <- use $ uiState . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. isWorldEditorEnabled
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    uiState . uiWorldEditor . paintedTerrain %= M.delete (mouseCoords ^. planar)
  immediatelyRedrawWorld
  return ()

-- | "Eye Dropper" tool:
handleMiddleClick :: B.Location -> EventM Name AppState ()
handleMiddleClick mouseLoc = do
  worldEditor <- use $ uiState . uiWorldEditor
  when (worldEditor ^. isWorldEditorEnabled) $ do
    w <- use $ gameState . multiWorld
    let setTerrainPaint coords = do
          let (terrain, maybeElementPaint) =
                EU.getContentAt
                  worldEditor
                  w
                  coords
          uiState . uiWorldEditor . terrainList %= BL.listMoveToElement terrain
          forM_ maybeElementPaint $ \elementPaint ->
            let p = case elementPaint of
                  Facade efd -> efd
                  Ref r -> mkFacade r
             in uiState . uiWorldEditor . entityPaintList %= BL.listMoveToElement p

    mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    whenJust mouseCoordsM setTerrainPaint

-- | Handle user input events in the robot panel.
handleWorldEditorPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWorldEditorPanelEvent = \case
  Key V.KEsc -> uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
  Key V.KEnter -> do
    fring <- use $ uiState . uiWorldEditor . editorFocusRing
    case focusGetCurrent fring of
      Just (WorldEditorPanelControl x) -> activateWorldEditorFunction x
      _ -> return ()
  ControlChar 's' -> saveMapFile
  CharKey '\t' -> uiState . uiWorldEditor . editorFocusRing %= focusNext
  Key V.KBackTab -> uiState . uiWorldEditor . editorFocusRing %= focusPrev
  _ -> return ()

-- | Return value: whether the cursor position should be updated
updateAreaBounds :: Maybe (Cosmo W.Coords) -> EventM Name AppState Bool
updateAreaBounds = \case
  Nothing -> return True
  Just mouseCoords -> do
    selectorStage <- use $ uiState . uiWorldEditor . editingBounds . boundsSelectionStep
    case selectorStage of
      UpperLeftPending -> do
        uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= LowerRightPending mouseCoords
        return False
      -- TODO (#1152): Validate that the lower-right click is below and to the right of
      -- the top-left coord and that they are within the same subworld
      LowerRightPending upperLeftMouseCoords -> do
        uiState . uiWorldEditor . editingBounds . boundsRect
          .= Just (fmap (,view planar mouseCoords) upperLeftMouseCoords)
        uiState . uiWorldEditor . lastWorldEditorMessage .= Nothing
        uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
        t <- liftIO $ getTime Monotonic
        uiState . uiWorldEditor . editingBounds . boundsPersistDisplayUntil .= t + TimeSpec 2 0
        setFocus WorldEditorPanel
        return False
      SelectionComplete -> return True

saveMapFile :: EventM Name AppState ()
saveMapFile = do
  worldEditor <- use $ uiState . uiWorldEditor
  maybeBounds <- use $ uiState . uiWorldEditor . editingBounds . boundsRect
  w <- use $ gameState . multiWorld
  let mapCellGrid = EU.getEditedMapRectangle worldEditor maybeBounds w

  let fp = worldEditor ^. outputFilePath
  maybeScenarioPair <- use $ uiState . scenarioRef
  liftIO $ Y.encodeFile fp $ constructScenario (fst <$> maybeScenarioPair) mapCellGrid

  uiState . uiWorldEditor . lastWorldEditorMessage .= Just "Saved."
