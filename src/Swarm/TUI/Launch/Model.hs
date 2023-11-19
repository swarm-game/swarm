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
import Control.Carrier.Throw.Either (runThrow)
import Control.Lens (makeLenses)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.Scenario.Status (ParameterizableLaunchParams (LaunchParams), ScenarioInfoPair, SerializableLaunchParams)
import Swarm.Game.State (LaunchParams, ValidatedLaunchParams, getRunCodePath, parseCodeFile)
import Swarm.Language.Pretty (prettyText)
import Swarm.TUI.Model.Name
import Swarm.Util.Effect (withThrow)

-- | Use this to store error messages
-- on individual fields
type EditingLaunchParams = LaunchParams (Either Text)

toSerializableParams :: ValidatedLaunchParams -> SerializableLaunchParams
toSerializableParams (LaunchParams seedValue (Identity codeToRun)) =
  LaunchParams seedValue $ pure $ getRunCodePath =<< codeToRun

fromSerializableParams :: SerializableLaunchParams -> IO EditingLaunchParams
fromSerializableParams (LaunchParams (Identity maybeSeedValue) (Identity maybeCodePath)) = do
  eitherCode <-
    runThrow . withThrow (prettyText @SystemFailure) $
      traverse parseCodeFile maybeCodePath
  return $ LaunchParams (Right maybeSeedValue) eitherCode

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
  , _editingParams :: EditingLaunchParams
  }

makeLenses ''LaunchOptions
