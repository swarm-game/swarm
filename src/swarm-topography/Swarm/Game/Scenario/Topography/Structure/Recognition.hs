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
-- The type parameters, `b`, and `a`, correspond
-- to 'Cell' and 'Entity', respectively.
data StructureRecognizer b a = StructureRecognizer
  { _automatons :: RecognizerAutomatons b a
  , _foundStructures :: FoundRegistry b a
  -- ^ Records the top-left corner of the found structure
  , _recognitionLog :: [SearchLog a]
  }
  deriving (Generic)

makeLenses ''StructureRecognizer
