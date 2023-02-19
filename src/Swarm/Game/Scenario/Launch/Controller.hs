module Swarm.Game.Scenario.Launch.Controller where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.FileBrowser
import Control.Lens
import Control.Monad.Except
import Graphics.Vty qualified as V
import Swarm.Game.Scenario.Launch.Model
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI

handleLaunchOptionsEvent ::
  ScenarioInfoPair ->
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleLaunchOptionsEvent siPair = \case
  Key V.KBackTab ->
    uiState . uiLaunchConfig . scenarioConfigFocusRing %= focusPrev
  Key V.KUp ->
    uiState . uiLaunchConfig . scenarioConfigFocusRing %= focusPrev
  CharKey '\t' -> 
    uiState . uiLaunchConfig . scenarioConfigFocusRing %= focusNext
  Key V.KDown -> 
    uiState . uiLaunchConfig . scenarioConfigFocusRing %= focusNext
    
  Key V.KEnter -> do
    fr <- use $ uiState . uiLaunchConfig . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl item)) -> case item of
        SeedSelector -> return ()
        ScriptSelector -> do
          fb <-
            liftIO $
              newFileBrowser
                selectNonDirectories
                (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
                Nothing
          uiState . uiLaunchConfig . fileBrowser .= Just fb
        StartGameButton -> do
          closeModal
          startGame siPair Nothing
      _ -> return ()
  Key V.KEsc -> closeModal
  CharKey 'q' -> closeModal
  ControlChar 'q' -> closeModal
  ev -> do
    fr <- use $ uiState . uiLaunchConfig . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl item)) -> case item of
        SeedSelector -> Brick.zoom (uiState . uiLaunchConfig . seedValueEditor) (handleEditorEvent ev)
        ScriptSelector -> case ev of
          VtyEvent e ->
            Brick.zoom (uiState . uiLaunchConfig . fileBrowser . _Just) (handleFileBrowserEvent e)
          _ -> return ()
        StartGameButton -> return ()
      _ -> return ()
 where
  closeModal = do
    fb <- use $ uiState . uiLaunchConfig . fileBrowser
    if null fb
      then uiState . uiLaunchConfig . isDisplayedFor .= Nothing
      else uiState . uiLaunchConfig . fileBrowser .= Nothing
