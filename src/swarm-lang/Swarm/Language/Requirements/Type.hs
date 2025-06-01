-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A requirement is something that is needed in order to successfully
-- build a robot running a certain program.
module Swarm.Language.Requirements.Type (
  -- * Requirements

  -- ** The 'Requirement' type
  Requirement (..),

  -- ** The 'Requirements' type and utility functions
  Requirements (..),
  singleton,
  singletonCap,
  singletonDev,
  singletonInv,
  insert,
  ReqCtx,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Language.Capability (Capability (..))
import Swarm.Language.Context (Ctx)

-- | A /requirement/ is something a robot must have when it is
--   built. There are three types:
--   - A robot can require a certain 'Capability', which should be fulfilled
--     by equipping an appropriate device.
--   - A robot can require a specific /device/, which should be equipped.
--   - A robot can stock some number of a specific entity in its inventory.
data Requirement
  = -- | Require a specific capability.  This must be fulfilled by
    --   equipping an appropriate device.  Requiring the same
    --   capability multiple times is the same as requiring it once.
    ReqCap Capability
  | -- | Require a specific device to be equipped.  Note that at this
    --   point it is only a name, and has not been resolved to an actual
    --   'Swarm.Game.Entity.Entity'.  That's because programs have to be type- and
    --   capability-checked independent of an 'Swarm.Game.Entity.EntityMap'.  The name
    --   will be looked up at runtime, when actually executing a 'Swarm.Language.Syntax.Build'
    --   or 'Swarm.Language.Syntax.Reprogram' command, and an appropriate exception thrown if
    --   a device with the given name does not exist.
    --
    --   Requiring the same device multiple times is the same as
    --   requiring it once.
    ReqDev Text
  | -- | Stock a certain number of a specific entity to be available
    --   in the inventory.  The same comments apply re: resolving the
    --   entity name to an actual 'Swarm.Game.Entity.Entity'.
    --
    --   Inventory requirements are additive, that is, say, requiring 5
    --   of entity @"e"@ and later requiring 7 is the same as requiring
    --   12.
    ReqInv Int Text
  deriving (Eq, Ord, Show, Generic, Hashable, Data, FromJSON, ToJSON)

-- | It is tempting to define @Requirements = Set Requirement@, but
--   that would be wrong, since two identical 'ReqInv' should have
--   their counts added rather than simply being deduplicated.
--
--   Since we will eventually need to deal with the different types of
--   requirements separately, it makes sense to store them separately
--   anyway.
data Requirements = Requirements
  { capReqs :: Set Capability
  , devReqs :: Set Text
  , invReqs :: Map Text Int
  }
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON, Hashable)

instance Semigroup Requirements where
  Requirements c1 d1 i1 <> Requirements c2 d2 i2 =
    Requirements (c1 <> c2) (d1 <> d2) (M.unionWith (+) i1 i2)

instance Monoid Requirements where
  mempty = Requirements S.empty S.empty M.empty

-- | Create a 'Requirements' set with a single 'Requirement'.
singleton :: Requirement -> Requirements
singleton (ReqCap c) = Requirements (S.singleton c) S.empty M.empty
singleton (ReqDev d) = Requirements S.empty (S.singleton d) M.empty
singleton (ReqInv n e) = Requirements S.empty S.empty (M.singleton e n)

-- | For convenience, create a 'Requirements' set with a single
--   'Capability' requirement.
singletonCap :: Capability -> Requirements
singletonCap = singleton . ReqCap

-- | For convenience, create a 'Requirements' set with a single
--   device requirement.
singletonDev :: Text -> Requirements
singletonDev = singleton . ReqDev

-- | For convenience, create a 'Requirements' set with a single
--   inventory requirement.
singletonInv :: Int -> Text -> Requirements
singletonInv n e = singleton (ReqInv n e)

insert :: Requirement -> Requirements -> Requirements
insert = (<>) . singleton

-- | A requirement context records the requirements for the
--   definitions bound to variables.
type ReqCtx = Ctx Requirements
