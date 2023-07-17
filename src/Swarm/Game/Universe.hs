{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Universe where

import Control.Lens (makeLenses, view)
import Data.Function (on)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Yaml (FromJSON, ToJSON, Value (Object), parseJSON, withText, (.:))
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Location

data SubworldName = DefaultRootSubworld | SubworldName Text
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance FromJSON SubworldName where
  parseJSON = withText "subworld name" $ return . SubworldName

renderWorldName :: SubworldName -> Text
renderWorldName = \case
  SubworldName s -> s
  DefaultRootSubworld -> "<default>"

-- | The swarm universe consists of locations
-- indexed by subworld.
-- Not only is this datatype useful for planar (2D)
-- coordinates, but is also used for named waypoints.
data Cosmo a = Cosmo
  { _subworld :: SubworldName
  , _planar :: a
  }
  deriving (Show, Eq, Ord, Functor, Generic, ToJSON)

makeLenses ''Cosmo

instance (FromJSON a) => FromJSON (Cosmo a) where
  parseJSON x = case x of
    Object v -> objParse v
    _ -> Cosmo DefaultRootSubworld <$> parseJSON x
   where
    objParse v =
      Cosmo
        <$> v .: "subworld"
        <*> v .: "loc"

defaultCosmoLocation :: Cosmo Location
defaultCosmoLocation = Cosmo DefaultRootSubworld origin

data DistanceMeasure b = Measurable b | InfinitelyFar
  deriving (Eq, Ord)

-- | Returns 'InfinitelyFar' if not within the same subworld.
cosmoMeasure :: (a -> a -> b) -> Cosmo a -> Cosmo a -> DistanceMeasure b
cosmoMeasure f a b
  | ((/=) `on` view subworld) a b = InfinitelyFar
  | otherwise = Measurable $ (f `on` view planar) a b

offsetBy :: Cosmo Location -> V2 Int32 -> Cosmo Location
offsetBy loc v = fmap (.+^ v) loc
