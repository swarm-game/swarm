module Swarm.TUI.Launch.Controller where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.FileBrowser
import Control.Lens
import Graphics.Vty qualified as V
import Swarm.TUI.Launch.Model
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI

handleFBEvent ::
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleFBEvent = \case
  Key V.KEsc -> closeModal
  CharKey 'q' -> closeModal
  ControlChar 'q' -> closeModal
  VtyEvent e ->
    Brick.zoom (uiState . uiLaunchConfig . fileBrowser . fbWidget) (handleFileBrowserEvent e)
  _ -> return ()
 where
  closeModal = uiState . uiLaunchConfig . fileBrowser . fbIsDisplayed .= False

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
  CharKey ' ' -> activateControl
  Key V.KEnter -> activateControl
  Key V.KEsc -> closeModal
  CharKey 'q' -> closeModal
  ControlChar 'q' -> closeModal
  ev -> do
    fr <- use $ uiState . uiLaunchConfig . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl SeedSelector)) ->
        Brick.zoom (uiState . uiLaunchConfig . seedValueEditor) (handleEditorEvent ev)
      _ -> return ()
 where
  activateControl = do
    fr <- use $ uiState . uiLaunchConfig . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl item)) -> case item of
        SeedSelector -> return ()
        ScriptSelector ->
          uiState . uiLaunchConfig . fileBrowser . fbIsDisplayed .= True
        StartGameButton -> do
          closeModal
          startGameWithSeed Nothing siPair Nothing
      _ -> return ()

  closeModal = uiState . uiLaunchConfig . isDisplayedFor .= Nothing
