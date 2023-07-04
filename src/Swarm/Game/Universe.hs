{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Universe where

import Control.Lens (makeLenses, view)
import Control.Monad (guard)
import Data.Function (on)
import Data.Text (Text)
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)

rootSubworldName :: SubworldName
rootSubworldName = SubworldName "root"

-- | Note: The primary overworld shall use
-- the reserved name \"root\".
newtype SubworldName = SubworldName Text
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | The swarm universe consists of planar locations
-- indexed by subworld.
data Cosmo a = Cosmo
  { _subworld :: SubworldName
  , _planar :: a
  }
  deriving (Show, Eq, Ord, Functor, Generic, FromJSON, ToJSON)

makeLenses ''Cosmo

-- | Returns "Nothing" if not within the same subworld.
cosmoMeasure :: (a -> a -> b) -> Cosmo a -> Cosmo a -> Maybe b
cosmoMeasure f a b = do
  guard $ ((==) `on` view subworld) a b
  pure $ (f `on` view planar) a b
