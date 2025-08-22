{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- World update result record.
module Swarm.Game.World.Update (
  WorldUpdate (..),
) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Swarm.Game.Location
import Swarm.Game.Universe

data WorldUpdate e = ReplaceEntity
  { updatedLoc :: Cosmic Location
  , originalEntity :: e
  , newEntity :: Maybe e
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
