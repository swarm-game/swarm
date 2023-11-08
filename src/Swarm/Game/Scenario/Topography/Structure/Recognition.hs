{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Public interface for structure recognizer.
module Swarm.Game.Scenario.Topography.Structure.Recognition where

import Control.Lens
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type

data StructureRecognizer = StructureRecognizer
  { _automatons :: RecognizerAutomatons
  , _foundStructures :: FoundRegistry
  -- ^ Records the top-left corner of the found structure
  , _recognitionLog :: [SearchLog]
  }
  deriving (Generic)

makeLenses ''StructureRecognizer
