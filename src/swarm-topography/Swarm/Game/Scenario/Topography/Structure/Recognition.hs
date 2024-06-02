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

-- |
-- The three type parameters, `b`, `en`, and `a`, correspond
-- to 'Cell', 'EntityName', and 'Entity', respectively.
data StructureRecognizer b en a = StructureRecognizer
  { _automatons :: RecognizerAutomatons b en a
  , _foundStructures :: FoundRegistry b a
  -- ^ Records the top-left corner of the found structure
  , _recognitionLog :: [SearchLog en]
  }
  deriving (Generic)

makeLenses ''StructureRecognizer
