{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- -Wno-orphans is for the ToJSON/FromJSON TimeSpec instances

-- |
-- Module      :  Swarm.Game.ScenarioStatus
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code to save and restore the status of scenarios, i.e. which have been
-- started, which have been completed, etc.
module Swarm.Game.ScenarioStatus (
  ScenarioStatus (..),
  ScenarioInfo,
  ssPath,
  ssStatus,
) where

import Control.Lens (
  Lens',
  generateSignatures,
  lensRules,
  makeLensesWith,
  (&),
  (.~),
 )
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Clock (TimeSpec)

deriving instance FromJSON TimeSpec
deriving instance ToJSON TimeSpec

data ScenarioStatus
  = NotStarted
  | InProgress ZonedTime -- Time started
  | Complete ZonedTime NominalDiffTime -- Time ended and elapsed time
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data ScenarioInfo = ScenarioInfo
  { _ssPath :: FilePath
  , _ssStatus :: ScenarioStatus
  }
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''ScenarioInfo

-- | The path of the scenario, relative to @data/scenarios@.
ssPath :: Lens' ScenarioInfo FilePath

-- | The status of the scenario.
ssStatus :: Lens' ScenarioInfo ScenarioStatus
