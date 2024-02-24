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
  getCapabilitySet,
  zeroCostCapabilities,
  transformIngredients,
  promoteDeviceUseCost,
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
import Swarm.Language.Capability (Capability)

-- This wrapper exists so that YAML can be parsed
-- either as a list of 'Capability' or as a Map.
newtype Capabilities e = Capabilities
  { getMap :: Map Capability e
  }
  deriving (Show, Eq, Generic, ToJSON, Hashable, Functor)

getCapabilitySet :: Capabilities e -> Set Capability
getCapabilitySet (Capabilities m) = M.keysSet m

zeroCostCapabilities :: Set Capability -> Capabilities (ExerciseCost e)
zeroCostCapabilities = Capabilities . M.fromSet (const $ ExerciseCost [])

type SingleEntityCapabilities e = Capabilities (ExerciseCost e)

type MultiEntityCapabilities e en = Capabilities (NonEmpty (DeviceUseCost e en))

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
  deriving (Eq, Show, Generic, FromJSON, ToJSON, Hashable, Functor)

instance (Eq e) => Ord (ExerciseCost e) where
  compare = compare `on` (getCost . ingredients)

data DeviceUseCost e en = DeviceUseCost
  { device :: e
  , useCost :: ExerciseCost en
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Functor)

-- TODO Should this derive from an Applicative instance?
promoteDeviceUseCost ::
  Monad m =>
  (e -> m e') ->
  DeviceUseCost x e ->
  m (DeviceUseCost x e')
promoteDeviceUseCost f (DeviceUseCost d ex) =
  DeviceUseCost d <$> transformIngredients f ex

-- TODO Should this derive from an Applicative instance?
transformIngredients ::
  Monad m =>
  (e -> m e') ->
  ExerciseCost e ->
  m (ExerciseCost e')
transformIngredients f (ExerciseCost ings) =
  ExerciseCost <$> mapM (traverse f) ings
