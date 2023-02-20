module Swarm.TUI.Launch.Controller where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.FileBrowser
import Control.Lens
import Control.Monad.Except (liftIO)
import Graphics.Vty qualified as V
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Controller.Util
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep (toValidatedParms)
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
    Brick.zoom (uiState . uiLaunchConfig . controls . fileBrowser . fbWidget) (handleFileBrowserEvent e)
  _ -> return ()
 where
  closeModal = uiState . uiLaunchConfig . controls . fileBrowser . fbIsDisplayed .= False

handleLaunchOptionsEvent ::
  ScenarioInfoPair ->
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleLaunchOptionsEvent siPair = \case
  Key V.KBackTab ->
    uiState . uiLaunchConfig . controls . scenarioConfigFocusRing %= focusPrev
  Key V.KUp ->
    uiState . uiLaunchConfig . controls . scenarioConfigFocusRing %= focusPrev
  CharKey '\t' ->
    uiState . uiLaunchConfig . controls . scenarioConfigFocusRing %= focusNext
  Key V.KDown ->
    uiState . uiLaunchConfig . controls . scenarioConfigFocusRing %= focusNext
  CharKey ' ' -> activateControl
  Key V.KEnter -> activateControl
  Key V.KEsc -> closeModal
  CharKey 'q' -> closeModal
  ControlChar 'q' -> closeModal
  ev -> do
    fr <- use $ uiState . uiLaunchConfig . controls . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl SeedSelector)) ->
        Brick.zoom (uiState . uiLaunchConfig . controls . seedValueEditor) (handleEditorEvent ev)
      _ -> return ()
 where
  activateControl = do
    fr <- use $ uiState . uiLaunchConfig . controls . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl item)) -> case item of
        SeedSelector -> return ()
        ScriptSelector ->
          uiState . uiLaunchConfig . controls . fileBrowser . fbIsDisplayed .= True
        StartGameButton -> do
          launchControls <- use $ uiState . uiLaunchConfig . controls
          eitherLaunchParams <- liftIO $ toValidatedParms launchControls
          case eitherLaunchParams of
            Left errMsg -> return () -- TODO FIXME
            Right launchParams -> do
              closeModal
              startGameWithSeed siPair launchParams
      _ -> return ()

  closeModal = uiState . uiLaunchConfig . controls . isDisplayedFor .= Nothing
