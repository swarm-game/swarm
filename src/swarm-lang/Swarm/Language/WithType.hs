{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Language.WithType (WithType (..), value, polytype, requires) where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Types (Polytype)

-- | A value, or a hole, or something else that has its type & requirements fixed
data WithType v = WithType
  { _value :: v
  , _polytype :: Polytype
  , _requires :: Requirements
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeLenses ''WithType
