{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Universe where

import Data.Text (Text)
import Control.Lens (makeLenses)
import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON)

rootSubworldName :: SubworldName
rootSubworldName = SubworldName "root"

-- | Note: The primary overworld shall use
-- the reserved name \"root\".
newtype SubworldName = SubworldName Text
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | The swarm universe consists of planar locations
-- indexed by subworld.
data Cosmo a = Cosmo {
    _subworld :: SubworldName
  , _planar :: a
  } deriving (Show, Eq, Ord, Functor, Generic, FromJSON, ToJSON)

makeLenses ''Cosmo