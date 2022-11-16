module Swarm.TUI.Editor.EditorController where

import Brick hiding (Direction)
import Brick.Focus
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import Swarm.Game.State
import Swarm.TUI.Controller.ControllerUtils
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model

------------------------------------------------------------
-- World Editor panel events
------------------------------------------------------------

activateWorldEditorFunction :: WorldEditorFocusable -> EventM Name AppState ()
activateWorldEditorFunction BrushSelector = openModal TerrainPaletteModal
activateWorldEditorFunction EntitySelector =
  liftIO $ putStrLn "TODO"
activateWorldEditorFunction AreaSelector = do
  selectorStage <- use $ uiState . uiWorldEditor . boundsSelectionStep
  case selectorStage of
    SelectionComplete -> uiState . uiWorldEditor . boundsSelectionStep .= UpperLeftPending
    _ -> return ()
activateWorldEditorFunction OutputPathSelector =
  liftIO $ putStrLn "File selection"

-- | Handle user input events in the robot panel.
handleWorldEditorPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWorldEditorPanelEvent = \case
  Key V.KEsc -> uiState . uiWorldEditor . boundsSelectionStep .= SelectionComplete
  Key V.KEnter -> do
    fring <- use $ uiState . uiWorldEditor . editorFocusRing
    case focusGetCurrent fring of
      Just (WorldEditorPanelControl x) -> activateWorldEditorFunction x
      _ -> return ()
  ControlChar 's' -> do
    worldEditor <- use $ uiState . uiWorldEditor
    let fp = worldEditor ^. outputFilePath
    maybeBounds <- use $ uiState . uiWorldEditor . editingBounds
    w <- use $ gameState . world
    liftIO $ writeFile fp $ EU.getEditedMapAsString worldEditor maybeBounds w
  CharKey '\t' -> uiState . uiWorldEditor . editorFocusRing %= focusNext
  Key V.KBackTab -> uiState . uiWorldEditor . editorFocusRing %= focusPrev
  _ -> return ()
