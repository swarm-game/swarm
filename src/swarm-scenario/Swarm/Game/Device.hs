-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A device is an entity that provides capabilities.
module Swarm.Game.Device where

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Semigroup (Min (..))
import Data.Vector qualified as V
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Game.Ingredients
import Swarm.Language.Capability (Capability)

-- This wrapper exists so that YAML can be parsed
-- either as a list of 'Capability' or as a Map.
newtype Capabilities e = Capabilities
  { getMap :: Map Capability (Min (ExerciseCost e))
  }
  deriving (Show, Eq, Generic, ToJSON, Hashable)

-- | For JSON parsing only
data CapabilityCost e = CapabilityCost
  { capability :: Capability
  , cost :: IngredientList e
  }
  deriving (Generic, FromJSON)

instance (FromJSON e) => FromJSON (Capabilities e) where
  parseJSON x =
    Capabilities <$> (simpleList <|> costMap)
   where
    simpleList = M.fromSet (const $ pure $ ExerciseCost []) <$> parseJSON x
    costMap = withArray "Capabilities" (fmap (M.fromList . map toMapEntry) . mapM parseJSON . V.toList) x
    toMapEntry (CapabilityCost a b) = (a, pure $ ExerciseCost b)

instance (Ord e, Semigroup e) => Semigroup (Capabilities e) where
  Capabilities c1 <> Capabilities c2 =
    Capabilities $ M.unionWith (<>) c1 c2

instance (Ord e, Semigroup e) => Monoid (Capabilities e) where
  mempty = Capabilities mempty

-- | Exercising a capability may have a cost.
newtype ExerciseCost e = ExerciseCost
  { ingredients :: IngredientList e
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, Hashable)

instance (Eq e) => Ord (ExerciseCost e) where
  compare = compare `on` (getCost . ingredients)

-- TODO Intended to be used as follows:
-- Map Capability [DeviceUseCost Entity EntityName]
data DeviceUseCost e en = DeviceUseCost
  { useCost :: ExerciseCost en
  , device :: e
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
