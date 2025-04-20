-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Event handling for the scenario launch configuration dialog.
module Swarm.TUI.Launch.Controller where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.FileBrowser
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List.Extra (enumerate)
import Data.Maybe (listToMaybe)
import Graphics.Vty qualified as V
import Swarm.Game.Scenario.Status (ParameterizableLaunchParams (LaunchParams))
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Controller.Util
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep (initFileBrowserWidget, makeFocusRingWith, parseSeedInput, parseWidgetParams, toValidatedParams)
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI

updateFocusRing :: EditingLaunchParams -> EventM Name LaunchOptions ()
updateFocusRing parsedParams = do
  currentRing <- use $ controls . scenarioConfigFocusRing
  let eitherLaunchParams = toValidatedParams parsedParams
      modifyRingMembers = case eitherLaunchParams of
        Left _ -> filter (/= StartGameButton)
        Right _ -> id
      maybeCurrentFocus = focusGetCurrent currentRing
      refocusRing = maybe id focusSetCurrent maybeCurrentFocus

  controls . scenarioConfigFocusRing .= refocusRing (makeFocusRingWith $ modifyRingMembers enumerate)

cacheValidatedInputs :: EventM Name LaunchOptions ()
cacheValidatedInputs = do
  launchControls <- use controls
  parsedParams <- liftIO $ parseWidgetParams launchControls
  editingParams .= parsedParams
  updateFocusRing parsedParams

-- | This is split out from the combined parameter-validation function
-- because validating the seed is cheap, and shouldn't have to pay
-- the cost of re-parsing script code as the user types in the seed
-- selection field.
cacheValidatedSeedInput :: EventM Name LaunchOptions ()
cacheValidatedSeedInput = do
  seedEditor <- use $ controls . seedValueEditor
  let eitherMaybeSeed = parseSeedInput seedEditor
  LaunchParams _ eitherParsedCode <- use editingParams
  let newParams = LaunchParams eitherMaybeSeed eitherParsedCode
  editingParams .= newParams
  updateFocusRing newParams

-- | If the 'FileBrowser' is in "search mode", then we allow
-- more of the key events to pass through. Otherwise,
-- we intercept things like "q" (for quit) and Space (so that
-- we can restrict file selection to at most one).
handleFBEvent ::
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleFBEvent ev = do
  fb <- use $ uiState . uiLaunchConfig . controls . fileBrowser . fbWidget
  let isSearching = fileBrowserIsSearching fb
  case (isSearching, ev) of
    (False, Key V.KEsc) -> closeModal
    (False, CharKey 'q') -> closeModal
    (False, ControlChar 'q') -> closeModal
    -- Intercept the "space" key so that it cannot be used to select files
    -- (see note below).
    (False, CharKey ' ') -> return ()
    (_, VtyEvent e) -> do
      (shouldClose, maybeSingleFile) <- Brick.zoom (uiState . uiLaunchConfig . controls . fileBrowser . fbWidget) $ do
        handleFileBrowserEvent e
        -- If the browser has a selected file after handling the
        -- event (because the user pressed Enter), close the dialog.
        case e of
          V.EvKey V.KEnter [] -> do
            b' <- get
            case FB.fileBrowserSelection b' of
              [] -> return (False, Nothing)
              -- We only allow one file to be selected
              -- by closing immediately.
              -- This is a hack illustrated in the Brick FileBrowser demo:
              -- https://github.com/jtdaugherty/brick/blob/4b40476d5d58c40720170d21503c11596bc9ee39/programs/FileBrowserDemo.hs#L68-L69
              -- It is not foolproof on its own, so we also intercept
              -- the "Space" key above.
              xs -> return (True, FB.fileInfoFilePath <$> listToMaybe xs)
          -- NOTE: The "Space" key also selects a file.
          -- Apparently, even when directories are specified as
          -- non-selectable via "FB.selectNonDirectories", the internal state
          -- of the FileBrowser dialog
          -- briefly adds a directory to its "fileBrowserSelection" list
          -- when the "space" key is pressed.
          -- So it is not enough to simply check whether the selection list
          -- is nonempty after *any* keypress; we specifically have to listen for "Enter".
          --
          -- WARNING: There is still a bug when one presses the "space" key to mark
          -- a directory, then presses "Enter" right afterward.
          -- The directory will get selected, and then swarm will crash.
          -- This is why we prevent the Space key from being handled by the FileBrowser
          -- unless we are in file searching mode.
          _ -> return (False, Nothing)

      when shouldClose $ do
        uiState . uiLaunchConfig . controls . fileBrowser . maybeSelectedFile .= maybeSingleFile
        closeModal
    _ -> return ()
 where
  closeModal = Brick.zoom (uiState . uiLaunchConfig) $ do
    controls . fileBrowser . fbIsDisplayed .= False
    cacheValidatedInputs

handleLaunchOptionsEvent ::
  ScenarioWith ScenarioInfo ->
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
  MouseDown n _ _ _ ->
    case n of
      ScenarioConfigControl (ScenarioConfigPanelControl x) -> do
        uiState . uiLaunchConfig . controls . scenarioConfigFocusRing %= focusSetCurrent n
        activateFocusedControl x
      _ -> return ()
  CharKey ' ' -> activateControl
  Key V.KEnter -> activateControl
  Key V.KEsc -> closeModal
  CharKey 'q' -> closeModal
  ControlChar 'q' -> closeModal
  ev -> do
    fr <- use $ uiState . uiLaunchConfig . controls . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl SeedSelector)) -> Brick.zoom (uiState . uiLaunchConfig) $ do
        Brick.zoom (controls . seedValueEditor) (handleEditorEvent ev)
        cacheValidatedSeedInput
      _ -> return ()
 where
  activateControl = do
    fr <- use $ uiState . uiLaunchConfig . controls . scenarioConfigFocusRing
    case focusGetCurrent fr of
      Just (ScenarioConfigControl (ScenarioConfigPanelControl item)) ->
        activateFocusedControl item
      _ -> return ()

  activateFocusedControl item = case item of
    SeedSelector -> return ()
    ScriptSelector -> Brick.zoom (uiState . uiLaunchConfig . controls . fileBrowser) $ do
      maybeSingleFile <- use maybeSelectedFile
      configuredFB <- initFileBrowserWidget maybeSingleFile
      fbWidget .= configuredFB
      fbIsDisplayed .= True
    StartGameButton -> do
      params <- use $ uiState . uiLaunchConfig . editingParams
      let eitherLaunchParams = toValidatedParams params
      forM_ eitherLaunchParams $ \launchParams -> do
        closeModal
        startGameWithSeed siPair launchParams

  closeModal = uiState . uiLaunchConfig . controls . isDisplayedFor .= Nothing
