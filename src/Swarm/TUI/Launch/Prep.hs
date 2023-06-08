{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use <$>" -}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Prepares and validates scenario launch parameters
module Swarm.TUI.Launch.Prep where

import Brick (EventM)
import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Arrow (left)
import Control.Lens ((.=), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Identity (runIdentity)
import Data.Text qualified as T
import Swarm.Game.Scenario.Status (ParameterizableLaunchParams (..), ScenarioInfoPair, getLaunchParams, scenarioStatus)
import Swarm.Game.State (getRunCodePath)
import Swarm.TUI.Launch.Model
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)
import System.FilePath (takeDirectory)
import Text.Read (readEither)

swarmLangFileExtension :: String
swarmLangFileExtension = "sw"

toValidatedParams :: EditingLaunchParams -> Either T.Text ValidatedLaunchParams
toValidatedParams (LaunchParams eitherSeedVal eitherInitialCode) = do
  maybeSeed <- eitherSeedVal
  maybeParsedCode <- eitherInitialCode
  return $ LaunchParams (pure maybeSeed) (pure maybeParsedCode)

parseWidgetParams :: LaunchControls -> IO EditingLaunchParams
parseWidgetParams (LaunchControls (FileBrowserControl _fb maybeSelectedScript _) seedEditor _ _) = do
  eitherParsedCode <- parseCode maybeSelectedScript
  return $ LaunchParams eitherMaybeSeed eitherParsedCode
 where
  eitherMaybeSeed =
    if T.null seedFieldText
      then Right Nothing
      else
        fmap Just
          . left T.pack
          . readEither
          . T.unpack
          $ seedFieldText

  seedFieldText = mconcat $ getEditContents seedEditor

makeFocusRingWith :: [ScenarioConfigPanelFocusable] -> Focus.FocusRing Name
makeFocusRingWith = Focus.focusRing . map (ScenarioConfigControl . ScenarioConfigPanelControl)

initEditorWidget :: T.Text -> Editor T.Text Name
initEditorWidget =
  editorText
    (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
    (Just 1) -- only allow a single line

-- | Called before any particular scenario is selected, so we
-- supply some "Nothing"s as defaults to the "ValidatedLaunchParams".
initConfigPanel :: IO LaunchOptions
initConfigPanel = do
  -- NOTE: This is kind of pointless, because we must re-instantiate the FileBrowser
  -- when it is first displayed, anyway.
  fb <-
    FB.newFileBrowser
      FB.selectNonDirectories
      (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
      Nothing -- Initial working directory to display
  return $
    LaunchOptions
      (LaunchControls (FileBrowserControl fb Nothing False) myForm ring Nothing)
      (LaunchParams (Right Nothing) (Right Nothing))
 where
  myForm = initEditorWidget ""
  ring = makeFocusRingWith listEnums

initFileBrowserWidget ::
  MonadIO m =>
  Maybe FilePath ->
  m (FB.FileBrowser Name)
initFileBrowserWidget maybePlayedScript = do
  fb <-
    liftIO $
      FB.newFileBrowser
        FB.selectNonDirectories
        (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
        (takeDirectory <$> maybePlayedScript) -- Initial working directory to display
  return $ FB.setFileBrowserEntryFilter (Just $ FB.fileExtensionMatch swarmLangFileExtension) fb

-- | If the selected scenario has been launched with an initial script before,
-- set the file browser to initially open that script's directory.
-- Then set the launch dialog to be displayed.
--
-- Note that the FileBrowser widget normally allows multiple selections ("marked" files).
-- However, there do not exist any public "setters" set the marked files, so we have
-- some workarounds:
-- * When the user marks the first file, we immediately close the FileBrowser widget.
-- * We re-instantiate the FileBrowser from scratch every time it is opened, so that
--   it is not possible to mark more than one file.
-- * The "marked file" is persisted outside of the FileBrowser state, and the
--   "initial directory" is set upon instantiation from that external state.
prepareLaunchDialog ::
  ScenarioInfoPair ->
  EventM Name LaunchOptions ()
prepareLaunchDialog siPair@(_, si) = do
  let serializableLaunchParams = getLaunchParams $ si ^. scenarioStatus
  launchEditingParams <- liftIO $ fromSerializableParams serializableLaunchParams
  editingParams .= launchEditingParams

  let maybePlayedScript = case initialCode launchEditingParams of
        Right codeToRun -> getRunCodePath =<< codeToRun
        Left _ -> Nothing

  controls . fileBrowser . maybeSelectedFile .= maybePlayedScript
  controls . seedValueEditor .= initEditorWidget (maybe "" (T.pack . show) $ runIdentity $ seedVal serializableLaunchParams)
  controls . isDisplayedFor .= Just siPair
