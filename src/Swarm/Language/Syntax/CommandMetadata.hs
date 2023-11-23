-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Command metadata.
module Swarm.Language.Syntax.CommandMetadata where

import Data.Aeson (ToJSON)
import Data.Set (Set)
import GHC.Generics (Generic)

data SensingType
  = RobotSensing
  | EntitySensing
  | WorldCondition
  deriving (Eq, Ord, Show, Generic, ToJSON)

data QueryType
  = -- | empirical knowledge
    Sensing SensingType
  | -- | a priori knowledge
    APriori
  deriving (Eq, Ord, Show, Generic, ToJSON)

data RobotChangeType
  = PositionChange
  | InventoryChange
  | ExistenceChange
  | BehaviorChange
  deriving (Eq, Ord, Show, Generic, ToJSON)

data MutationType
  = Cosmetic
  | EntityChange
  | RobotChange RobotChangeType
  deriving (Eq, Ord, Show, Generic, ToJSON)

data CommandEffect
  = Computation
  | Query QueryType
  | MetaEffect
  | Mutation (Set MutationType)
  deriving (Eq, Ord, Show, Generic, ToJSON)
