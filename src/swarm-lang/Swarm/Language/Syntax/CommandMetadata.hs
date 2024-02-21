-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Command metadata for documentation
module Swarm.Language.Syntax.CommandMetadata where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data SensingType
  = RobotSensing
  | EntitySensing
  | WorldCondition
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, ToJSON)

data QueryType
  = -- | empirical knowledge
    Sensing SensingType
  | -- | The random number generator
    PRNG
  | -- | a priori knowledge
    APriori
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RobotChangeType
  = PositionChange
  | InventoryChange
  | ExistenceChange
  | BehaviorChange
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, ToJSON)

data MutationType
  = Cosmetic
  | LogEmission
  | EntityChange
  | RobotChange RobotChangeType
  deriving (Eq, Ord, Show, Generic, ToJSON)

data CommandEffect
  = Query QueryType
  | MetaEffect
  | Mutation MutationType
  deriving (Eq, Ord, Show, Generic, ToJSON)
