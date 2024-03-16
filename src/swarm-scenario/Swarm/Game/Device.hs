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

-- | The 'Capabilities e' wrapper type stores information of type @e@ for each of some set of capabilities.
-- For example, @e@ could be a list of ingredients needed to exercise a capability, or a set of devices capable of providing a capability.
newtype Capabilities e = Capabilities
  { getMap :: Map Capability e
  }
  deriving (Show, Eq, Generic, ToJSON, Hashable, Functor, Foldable, Traversable)

getCapabilitySet :: Capabilities e -> Set Capability
getCapabilitySet (Capabilities m) = M.keysSet m

type SingleEntityCapabilities e = Capabilities (ExerciseCost e)

type MultiEntityCapabilities e en = Capabilities (NonEmpty (DeviceUseCost e en))

zeroCostCapabilities :: Set Capability -> SingleEntityCapabilities e
zeroCostCapabilities = Capabilities . M.fromSet (const $ ExerciseCost [])

-- | For JSON parsing only
data CapabilityCost e = CapabilityCost
  { capability :: Capability
  , cost :: IngredientList e
  }
  deriving (Generic, FromJSON)

-- | First, attempt to parse capabilities as a list.
-- Otherwise, parse as a Map from capabilities to ingredients.
instance (FromJSON e) => FromJSON (SingleEntityCapabilities e) where
  parseJSON x =
    Capabilities <$> (simpleList <|> costMap)
   where
    simpleList = M.fromSet (const $ ExerciseCost []) <$> parseJSON x
    costMap = withArray "Capabilities" (fmap (M.fromList . map toMapEntry) . mapM parseJSON . V.toList) x
    toMapEntry (CapabilityCost a b) = (a, ExerciseCost b)

instance (Ord e, Semigroup e) => Semigroup (Capabilities e) where
  Capabilities c1 <> Capabilities c2 =
    Capabilities $ M.unionWith (<>) c1 c2

instance (Ord e, Semigroup e) => Monoid (Capabilities e) where
  mempty = Capabilities mempty

-- | Exercising a capability may have a cost.
newtype ExerciseCost e = ExerciseCost
  { ingredients :: IngredientList e
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, Hashable, Functor, Foldable, Traversable)

instance (Eq e) => Ord (ExerciseCost e) where
  compare = compare `on` (getCost . ingredients)

data DeviceUseCost e en = DeviceUseCost
  { device :: e
  , useCost :: ExerciseCost en
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Functor, Foldable, Traversable)

-- * Utils

data CommandsAndCost e = CommandsAndCost
  { commandCost :: ExerciseCost e
  , enabledCommands :: NonEmpty Const
  }

-- | NOTE: Because each 'Const' is mapped to at most one
-- 'Capability' by the 'constCaps' function, we know that
-- a given 'Const' will not appear more than once as a value in the 'Map' produced by
-- this function, i.e. for the  capabilities provided by a single 'Entity`
-- ('SingleEntityCapabilities').
commandsForDeviceCaps :: SingleEntityCapabilities e -> Capabilities (CommandsAndCost e)
commandsForDeviceCaps = Capabilities . M.mapMaybeWithKey f . getMap
 where
  f cap xc =
    CommandsAndCost xc <$> M.lookup cap constByCaps
