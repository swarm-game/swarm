{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use <$>" -}

-- | Prepares and validates scenario launch parameters
module Swarm.TUI.Launch.Prep where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Arrow (left)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Swarm.Game.State (parseCodeFile)
import Swarm.TUI.Launch.Model
import Swarm.TUI.Model.Name
import Swarm.Util (listEnums)
import Text.Read (readEither)

swarmLangFileExtension :: String
swarmLangFileExtension = "sw"

toValidatedParms :: EditingLaunchParms -> Either T.Text ValidatedLaunchParms
toValidatedParms (LaunchParms eitherSeedVal eitherInitialCode) = do
  maybeSeed <- eitherSeedVal
  maybeParsedCode <- eitherInitialCode
  return $ LaunchParms (pure maybeSeed) (pure maybeParsedCode)

parseWidgetParms :: LaunchControls -> IO EditingLaunchParms
parseWidgetParms (LaunchControls (FileBrowserControl fb _) seedEditor _ _) = do
  eitherParsedCode <- case maybeSelectedFile of
    Just codeFile -> do
      eitherParsedCode <- parseCodeFile codeFile
      return $ Just <$> eitherParsedCode
    Nothing -> return $ Right Nothing
  return $ LaunchParms eitherMaybeSeed eitherParsedCode
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

  -- NOTE: We only allow one file to be selected.
  maybeSelectedFile =
    FB.fileInfoFilePath
      <$> listToMaybe (FB.fileBrowserSelection fb)

makeFocusRingWith :: [ScenarioConfigPanelFocusable] -> Focus.FocusRing Name
makeFocusRingWith = Focus.focusRing . map (ScenarioConfigControl . ScenarioConfigPanelControl)

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
      (LaunchParms (Right Nothing) (Right Nothing))
 where
  myForm =
    editorText
      (ScenarioConfigControl $ ScenarioConfigPanelControl SeedSelector)
      (Just 1)
      ""
  ring = makeFocusRingWith listEnums
