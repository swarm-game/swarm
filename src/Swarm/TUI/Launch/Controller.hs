module Swarm.TUI.Launch.Controller where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.FileBrowser
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens
import Control.Monad.Except (forM_, liftIO, when)
import Graphics.Vty qualified as V
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Controller.Util
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep (toValidatedParms)
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI

cacheValidatedInputs :: EventM Name AppState ()
cacheValidatedInputs = do
  launchControls <- use $ uiState . uiLaunchConfig . controls
  eitherLaunchParams <- liftIO $ toValidatedParms launchControls
  uiState . uiLaunchConfig . validatedParams .= eitherLaunchParams

handleFBEvent ::
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleFBEvent = \case
  Key V.KEsc -> closeModal
  CharKey 'q' -> closeModal
  ControlChar 'q' -> closeModal
  VtyEvent e -> do
    shouldClose <- Brick.zoom (uiState . uiLaunchConfig . controls . fileBrowser . fbWidget) $ do
      handleFileBrowserEvent e
      -- If the browser has a selected file after handling the
      -- event (because the user pressed Enter), close the dialog.
      case e of
        V.EvKey V.KEnter [] -> do
          b' <- get
          return $ case FB.fileBrowserSelection b' of
            [] -> False
            _ -> True
        _ -> return False
    when shouldClose closeModal
  _ -> return ()
 where
  closeModal = do
    uiState . uiLaunchConfig . controls . fileBrowser . fbIsDisplayed .= False
    cacheValidatedInputs

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
      Just (ScenarioConfigControl (ScenarioConfigPanelControl SeedSelector)) -> do
        Brick.zoom (uiState . uiLaunchConfig . controls . seedValueEditor) (handleEditorEvent ev)
        cacheValidatedInputs
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
          eitherLaunchParams <- use $ uiState . uiLaunchConfig . validatedParams
          forM_ eitherLaunchParams $ \launchParams -> do
            closeModal
            startGameWithSeed siPair launchParams
      _ -> return ()

  closeModal = uiState . uiLaunchConfig . controls . isDisplayedFor .= Nothing
