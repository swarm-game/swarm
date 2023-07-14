{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Universe where

import Control.Lens (makeLenses, view)
import Control.Monad (guard)
import Data.Function (on)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Yaml (FromJSON, ToJSON, Value (Object), parseJSON, (.:))
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Location

-- TODO: It would be better not to use this as
-- an "in-band" reserved name. Preferably its use
-- should be eliminated entirely.
-- E.g., robot locations specified within a toplevel robot
-- definition (instead of on a map) should require the subworld
-- to be specified.  Currently, the subworld name is optional
-- for backwards compatibility.
-- If, after all, a "default" is still required, should
-- use a dedicated sum type member to designate
-- the "default" for cases in which it is required.
defaultRootSubworldName :: SubworldName
defaultRootSubworldName = SubworldName "root"

-- | Note: The primary overworld shall use
-- the reserved name \"root\".
newtype SubworldName = SubworldName Text
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

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
    _ -> Cosmo defaultRootSubworldName <$> parseJSON x
   where
    objParse v =
      Cosmo
        <$> v .: "subworld"
        <*> v .: "loc"

defaultCosmoLocation :: Cosmo Location
defaultCosmoLocation = Cosmo defaultRootSubworldName origin

-- | Returns 'Nothing' if not within the same subworld.
-- TODO: Define a new datatype isomorphic to Maybe for this.
cosmoMeasure :: (a -> a -> b) -> Cosmo a -> Cosmo a -> Maybe b
cosmoMeasure f a b = do
  guard $ ((==) `on` view subworld) a b
  pure $ (f `on` view planar) a b

offsetBy :: Cosmo Location -> V2 Int32 -> Cosmo Location
offsetBy loc v = fmap (.+^ v) loc
