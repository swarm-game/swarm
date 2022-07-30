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
  ssBuiltin,
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
  | -- Use TimeSpec so we can reliably compute the elapsed time, ClockTime
    -- for displaying to the user?
    InProgress TimeSpec {- ClockTime -}
  | Complete TimeSpec {- ClockTime -}
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data ScenarioInfo = ScenarioInfo
  { _ssPath :: FilePath
  , _ssBuiltin :: Bool
  , _ssStatus :: ScenarioStatus
  }
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

makeLensesWith (lensRules & generateSignatures .~ False) ''ScenarioInfo

-- | The path of the scenario.  For builtin/"blessed" scenarios, this
--   is interpreted as being relative to data/scenarios.  For other
--   scenarios, it is simply interpreted relative to the current
--   working directory (but is most likely stored as an absolute
--   path).
ssPath :: Lens' ScenarioInfo FilePath

-- | Whether the scenario is one of the builtin, "blessed" scenarios
--   or not.
ssBuiltin :: Lens' ScenarioInfo Bool

-- | The status of the scenario.
ssStatus :: Lens' ScenarioInfo ScenarioStatus
