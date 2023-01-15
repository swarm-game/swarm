{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for representing state of the launch dialog,
-- along with conversion functions for validated launch parameters.
module Swarm.TUI.Launch.Model where

import Brick.Focus qualified as Focus
import Brick.Widgets.Edit
import Brick.Widgets.FileBrowser qualified as FB
import Control.Lens (makeLenses)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import Swarm.Game.Scenario.Status (ParameterizableLaunchParams (LaunchParms), ScenarioInfoPair, SerializableLaunchParms)
import Swarm.Game.State (CodeToRun, getRunCodePath, parseCodeFile)
import Swarm.TUI.Model.Name

type LaunchParms a = ParameterizableLaunchParams CodeToRun a

-- | Use this to store error messages
-- on individual fields
type EditingLaunchParms = LaunchParms (Either Text)

-- | In this stage in the UI pipeline, both fields
-- have already been validated, and "Nothing" means
-- that the field is simply absent.
type ValidatedLaunchParms = LaunchParms Identity

toSerializableParams :: ValidatedLaunchParms -> SerializableLaunchParms
toSerializableParams (LaunchParms seedValue (Identity codeToRun)) =
  LaunchParms seedValue $ pure $ getRunCodePath =<< codeToRun

parseCode :: Maybe FilePath -> IO (Either Text (Maybe CodeToRun))
parseCode maybeSelectedFile = case maybeSelectedFile of
  Just codeFile -> do
    eitherParsedCode <- parseCodeFile codeFile
    return $ Just <$> eitherParsedCode
  Nothing -> return $ Right Nothing

fromSerializableParams :: SerializableLaunchParms -> IO EditingLaunchParms
fromSerializableParams (LaunchParms (Identity maybeSeedValue) (Identity maybeCodePath)) = do
  eitherCode <- parseCode maybeCodePath
  return $ LaunchParms (Right maybeSeedValue) eitherCode

data FileBrowserControl = FileBrowserControl
  { _fbWidget :: FB.FileBrowser Name
  , _maybeSelectedFile :: Maybe FilePath
  , _fbIsDisplayed :: Bool
  }

makeLenses ''FileBrowserControl

-- | UI elements to configure scenario launch options
data LaunchControls = LaunchControls
  { _fileBrowser :: FileBrowserControl
  , _seedValueEditor :: Editor Text Name
  , _scenarioConfigFocusRing :: Focus.FocusRing Name
  , _isDisplayedFor :: Maybe ScenarioInfoPair
  }

makeLenses ''LaunchControls

-- | UI elements to configure scenario launch options
data LaunchOptions = LaunchOptions
  { _controls :: LaunchControls
  , _editingParams :: EditingLaunchParms
  }

makeLenses ''LaunchOptions
