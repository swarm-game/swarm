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

-- | State of the structure recognizer that is intended
-- to be modifiable.
data RecognitionState b a = RecognitionState
  { _foundStructures :: FoundRegistry b a
  -- ^ Records the top-left corner of the found structure
  , _recognitionLog :: [SearchLog a]
  }

makeLenses ''RecognitionState

-- |
-- The type parameters, `b`, and `a`, correspond
-- to '(Structure.NamedGrid (Maybe Cell))' and 'Entity', respectively.
data StructureRecognizer b a = StructureRecognizer
  { _automatons :: RecognizerAutomatons b a
  -- ^ read-only
  , _recognitionState :: RecognitionState b a
  -- ^ mutatable
  }
  deriving (Generic)

makeLenses ''StructureRecognizer
