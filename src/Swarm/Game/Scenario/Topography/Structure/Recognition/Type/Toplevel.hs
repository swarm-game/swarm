{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer types: Public interface
module Swarm.Game.Scenario.Topography.Structure.Recognition.Type.Toplevel where

import Control.Lens hiding (from, (<.>))
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type

emptyFoundStructures :: FoundStructures
emptyFoundStructures = FoundStructures mempty mempty

data StructureRecognizer = StructureRecognizer
  { _automatons :: RecognizerAutomatons
  , _foundStructures :: FoundStructures
  -- ^ Records the top-left corner of the found structure
  , _recognitionLog :: [SearchLog]
  }
  deriving (Generic)

makeLenses ''StructureRecognizer
