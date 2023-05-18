{-# LANGUAGE OverloadedStrings #-}

-- | Prepares and validates scenario launch parameters
module Swarm.TUI.Launch.Prep where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Arrow (left)
import Control.Monad.Extra (pureIf)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, withExceptT)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Swarm.Game.State (parseCodeFile)
import Swarm.TUI.Launch.Model
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)
import Text.Read (readEither)

import Data.Functor.Identity (Identity (..))

swarmLangFileExtension :: String
swarmLangFileExtension = "sw"

toValidatedParms :: LaunchControls -> IO (Either LaunchFormError ValidatedLaunchParms)
toValidatedParms (LaunchControls (FileBrowserControl fb _) seedEditor _ _) = runExceptT $ do
  maybeParsedCode <-
    traverse
      (withExceptT (LaunchFormError ScriptSelector) . ExceptT . parseCodeFile)
      maybeSelectedFile

  maybeSeed <-
    traverse
      ( withExceptT (LaunchFormError SeedSelector)
          . except
          . left T.pack
          . readEither
          . T.unpack
      )
      $ pureIf (not $ T.null seedFieldText) seedFieldText

  return $ LaunchParms (Identity maybeSeed) (Identity maybeParsedCode)
 where
  seedFieldText = mconcat $ getEditContents seedEditor
  maybeSelectedFile =
    FB.fileInfoFilePath
      <$> listToMaybe (FB.fileBrowserSelection fb)

-- | Called before any particular scenario is selected, so we
-- supply some "Nothing"s as defaults to the "ValidatedLaunchParms".
initConfigPanel :: IO LaunchOptions
initConfigPanel = do
  fb <-
    FB.newFileBrowser
      FB.selectNonDirectories
      (ScenarioConfigControl $ ScenarioConfigPanelControl ScriptSelector)
      Nothing
  let configuredFB = FB.setFileBrowserEntryFilter (Just $ FB.fileExtensionMatch swarmLangFileExtension) fb
  return $
    LaunchOptions
      (LaunchControls (FileBrowserControl configuredFB False) myForm ring Nothing)
      (Right $ LaunchParms (Identity Nothing) (Identity Nothing))
 where
  myForm =
    editorText
      (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
      (Just 1)
      ""
  ring = Focus.focusRing $ map (ScenarioConfigControl . ScenarioConfigPanelControl) listEnums
