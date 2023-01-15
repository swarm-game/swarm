{-# LANGUAGE OverloadedStrings #-}

-- | Prepares and validates scenario launch parameters
module Swarm.TUI.Launch.Prep where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Arrow (left)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.State (parseCodeFile)
import Swarm.TUI.Launch.Model
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)
import Text.Read (readEither)

toValidatedParms :: LaunchControls -> IO (Either Text ValidatedLaunchParms)
toValidatedParms (LaunchControls (FileBrowserControl fb _) seedEditor _ _) = runExceptT $ do
  maybeParsedCode <- case maybeSelectedFile of
    Nothing -> return Nothing
    Just filePath -> do
      code <- ExceptT $ parseCodeFile filePath
      return $ Just code

  maybeSeed <-
    if T.null seedFieldText
      then return Nothing
      else do
        val <- except $ left T.pack $ readEither $ T.unpack seedFieldText
        return $ Just val

  return $ ValidatedLaunchParms maybeSeed maybeParsedCode
 where
  seedFieldText = mconcat $ getEditContents seedEditor
  maybeSelectedFile =
    FB.fileInfoFilePath
      <$> listToMaybe (FB.fileBrowserSelection fb)

initConfigPanel :: IO LaunchOptions
initConfigPanel = do
  fb <-
    FB.newFileBrowser
      FB.selectNonDirectories
      -- (const False)
      (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
      Nothing
  let configuredFB = FB.setFileBrowserEntryFilter (Just $ FB.fileExtensionMatch "sw") fb
  return $ LaunchOptions (LaunchControls (FileBrowserControl configuredFB False) myForm ring Nothing) (ValidatedLaunchParms Nothing Nothing)
 where
  myForm =
    editorText
      (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
      (Just 1)
      ""
  ring = Focus.focusRing $ map (ScenarioConfigControl . ScenarioConfigPanelControl) listEnums
