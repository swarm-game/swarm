{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.Typed (Typed (..), value, polytype, requires) where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)
import Swarm.Language.Requirement (Requirements)
import Swarm.Language.Types (Polytype)

-- | A value, or a hole, or something else that has its type & requirements fixed
data Typed v = Typed
  { _value :: v
  , _polytype :: Polytype
  , _requires :: Requirements
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeLenses ''Typed
