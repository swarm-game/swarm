{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Launch.Prep where

import Swarm.TUI.Launch.Model
import Control.Arrow (left)
import Text.Read (readEither)
import Data.Maybe (listToMaybe)
import Control.Monad.Trans.Except (except, runExceptT, ExceptT (..))
import Swarm.Game.WorldGen (Seed)
import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Swarm.Game.State (parseCodeFile, CodeToRun)
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens (makeLenses)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.ScenarioInfo
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)


toValidatedParms :: LaunchOptions -> IO (Either Text ValidatedLaunchParms)
toValidatedParms (LaunchOptions (FileBrowserControl fb _) seedEditor _ _) = runExceptT $ do
  maybeParsedCode <- case maybeSelectedFile of
    Nothing -> return Nothing
    Just filePath -> do
      code <- ExceptT $ parseCodeFile filePath
      return $ Just code

  maybeSeed <- if T.null seedFieldText
    then return Nothing
    else do
      val <- except $ left T.pack $ readEither $ T.unpack seedFieldText
      return $ Just val

  return $ ValidatedLaunchParms maybeSeed maybeParsedCode
  where
    seedFieldText = mconcat $ getEditContents seedEditor
    maybeSelectedFile = FB.fileInfoFilePath <$>
      listToMaybe (FB.fileBrowserSelection fb)

initConfigPanel :: IO LaunchOptions
initConfigPanel = do
  fb <-
    FB.newFileBrowser
      FB.selectNonDirectories
      -- (const False)
      (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
      Nothing
  let configuredFB = FB.setFileBrowserEntryFilter (Just $ FB.fileExtensionMatch "sw") fb
  return $ LaunchOptions (FileBrowserControl configuredFB False) myForm ring Nothing
 where
  myForm =
    editorText
      (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
      (Just 1)
      ""
  ring = Focus.focusRing $ map (ScenarioConfigControl . ScenarioConfigPanelControl) listEnums
