-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A device is an entity that provides capabilities.
--
-- Some capabilities have a cost to exercise.
-- Items will be consumed from the inventory for
-- invoking a command that utilizes a given capability.
module Swarm.Game.Device (
  SingleEntityCapabilities,
  MultiEntityCapabilities,
  Capabilities (..),
  DeviceUseCost (..),
  ExerciseCost (..),
  CommandsAndCost (..),
  getCapabilitySet,
  zeroCostCapabilities,
  commandsForDeviceCaps,
)
where

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Vector qualified as V
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Game.Ingredients
import Swarm.Language.Capability (Capability, constByCaps)
import Swarm.Language.Syntax (Const)

-- | The 'Capabilities e' wrapper type stores information of type @e@ for each
-- of some set of capabilities.
-- For example, @e@ could be a list of ingredients needed to exercise a
-- capability, or a set of devices capable of providing a capability.
newtype Capabilities e = Capabilities { getMap :: Map Capability e }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving newtype (Show, Eq, ToJSON, Hashable)

-- | Get the set of capabilities about which we are storing information.
getCapabilitySet :: Capabilities e -> Set Capability
getCapabilitySet (Capabilities m) = M.keysSet m

-- | Records an 'ExerciseCost', i.e. list of consumed ingredients, per capability that can be exercised.  This represents information about a single entity/device, which can provide multiple capabilities (with a different exercise cost for each).
type SingleEntityCapabilities e = Capabilities (ExerciseCost e)

-- | Records a list of devices capable of providing each capability;
-- along with each device is recorded the 'ExerciseCost' needed to use
-- that device to achieve the given capability.
--
-- See 'DeviceUseCost' for explanation of type parameters.
type MultiEntityCapabilities e en = Capabilities (NonEmpty (DeviceUseCost e en))

-- | Create a default 'SingleEntityCapabilities' map for a device which provides capabilities with no associated costs.
zeroCostCapabilities :: Set Capability -> SingleEntityCapabilities e
zeroCostCapabilities = Capabilities . M.fromSet (const $ ExerciseCost [])

-- | Package together a capability and exercise cost; only used temporarily for parsing this information from JSON format.
data CapabilityCost e = CapabilityCost
  { capability :: Capability
  , cost :: IngredientList e
  }
  deriving (Generic, FromJSON)

-- | First, attempt to parse capabilities as a list, interpreted as a set of capabilities with no exercise cost.
-- Otherwise, parse as a Map from capabilities to ingredients.
instance (FromJSON e) => FromJSON (SingleEntityCapabilities e) where
  parseJSON x =
    (Capabilities <$> costMap) <|> simpleList
   where
    simpleList = zeroCostCapabilities <$> parseJSON x
    costMap = withArray "Capabilities" (fmap (M.fromList . map toMapEntry) . mapM parseJSON . V.toList) x
    toMapEntry (CapabilityCost a b) = (a, ExerciseCost b)

instance (Ord e, Semigroup e) => Semigroup (Capabilities e) where
  Capabilities c1 <> Capabilities c2 =
    Capabilities $ M.unionWith (<>) c1 c2

instance (Ord e, Semigroup e) => Monoid (Capabilities e) where
  mempty = Capabilities mempty

-- | Exercising a capability may have a cost, in the form of entities that must be consumed each time it is used.
newtype ExerciseCost e = ExerciseCost { ingredients :: IngredientList e }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving newtype (Eq, Show, FromJSON, ToJSON, Hashable)

-- | Sort 'ExerciseCost's by the total count of ingredients consumed.
instance (Eq e) => Ord (ExerciseCost e) where
  compare = compare `on` (getCost . ingredients)

-- | A device paired with a cost to use it.
--
-- At scenario parse time, the type parameters @e@ and @en@ will stand for
-- 'Entity' and 'EntityName'.
-- This is because `ExerciseCost` is a member of the 'Entity' datatype, and
-- therefore can only refer to another 'Entity' by name before all 'Entity's
-- are parsed.
--
-- However, after parse time, we are able to look up actual 'Entity' objects
-- by name, and therefore can instantiate 'ExerciseCost' with 'Entity' as
-- the type parameter.
-- Then the two type parameters of 'DeviceUseCost' are both of 'Entity' type.
data DeviceUseCost e en = DeviceUseCost
  { device :: e
  , useCost :: ExerciseCost en
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Functor, Foldable, Traversable)

-- * Utils

-- | A nonempty list of commands together with an exercise cost for using any of them (typically these will be a list of commands all requiring the same capability).
data CommandsAndCost e = CommandsAndCost
  { commandCost :: ExerciseCost e
  , enabledCommands :: NonEmpty Const
  }

-- | Given mapping from capabilities to their exercise costs provided by a single device, turn it into an mapping from capabilities to their exercise cost and enabled commands.
--
-- NOTE: Because each 'Const' is mapped to at most one
-- 'Capability' by the 'constCaps' function, we know that
-- a given 'Const' will not appear more than once as a value in the 'Map' produced by
-- this function, i.e. for the  capabilities provided by a single 'Entity`
-- ('SingleEntityCapabilities').
commandsForDeviceCaps :: SingleEntityCapabilities e -> Capabilities (CommandsAndCost e)
commandsForDeviceCaps = Capabilities . M.mapMaybeWithKey f . getMap
 where
  f cap xc =
    CommandsAndCost xc <$> M.lookup cap constByCaps
