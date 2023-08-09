{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types and helper functions for working with directions
module Swarm.Language.Direction (
  -- * Directions
  Direction (..),
  AbsoluteDir (..),
  RelativeDir (..),
  PlanarRelativeDir (..),
  directionSyntax,
  isCardinal,
  allDirs,
) where

import Data.Aeson.Types hiding (Key)
import Data.Char qualified as C (toLower)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.List qualified as L (tail)
import Data.Text hiding (filter, length, map)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Util qualified as Util
import Witch.From (from)

------------------------------------------------------------
-- Directions
------------------------------------------------------------

-- | An absolute direction is one which is defined with respect to an
--   external frame of reference; robots need a compass in order to
--   use them.
--
-- NOTE: These values are ordered by increasing angle according to
-- the standard mathematical convention.
-- That is, the right-pointing direction, East, is considered
-- the "reference angle" and the order proceeds counter-clockwise.
-- See https://en.wikipedia.org/wiki/Polar_coordinate_system#Conventions
--
-- Do not alter this ordering, as there exist functions that depend on it
-- (e.g. 'nearestDirection' and 'relativeTo').
data AbsoluteDir = DEast | DNorth | DWest | DSouth
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, Enum, Bounded)

directionJsonModifier :: String -> String
directionJsonModifier = map C.toLower . L.tail

directionJsonOptions :: Options
directionJsonOptions =
  defaultOptions
    { constructorTagModifier = directionJsonModifier
    }

instance FromJSON AbsoluteDir where
  parseJSON = genericParseJSON directionJsonOptions

instance ToJSON AbsoluteDir where
  toJSON = genericToJSON directionJsonOptions

cardinalDirectionKeyOptions :: JSONKeyOptions
cardinalDirectionKeyOptions =
  defaultJSONKeyOptions
    { keyModifier = directionJsonModifier
    }

instance ToJSONKey AbsoluteDir where
  toJSONKey = genericToJSONKey cardinalDirectionKeyOptions

instance FromJSONKey AbsoluteDir where
  fromJSONKey = genericFromJSONKey cardinalDirectionKeyOptions

-- | A relative direction is one which is defined with respect to the
--   robot's frame of reference; no special capability is needed to
--   use them.
data RelativeDir = DPlanar PlanarRelativeDir | DDown
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON)

-- | Caution: Do not alter this ordering, as there exist functions that depend on it
-- (e.g. 'nearestDirection' and 'relativeTo').
data PlanarRelativeDir = DForward | DLeft | DBack | DRight
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, Enum, Bounded)

instance FromJSON PlanarRelativeDir where
  parseJSON = genericParseJSON directionJsonOptions

instance ToJSON PlanarRelativeDir where
  toJSON = genericToJSON directionJsonOptions

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = DAbsolute AbsoluteDir | DRelative RelativeDir
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, ToJSON, FromJSON)

-- | Direction name is generated from the deepest nested data constructor
-- e.g. 'DLeft' becomes "left"
directionSyntax :: Direction -> Text
directionSyntax d = toLower . T.tail . from $ case d of
  DAbsolute x -> show x
  DRelative x -> case x of
    DPlanar y -> show y
    _ -> show x

-- | Check if the direction is absolute (e.g. 'north' or 'south').
isCardinal :: Direction -> Bool
isCardinal = \case
  DAbsolute _ -> True
  _ -> False

allDirs :: [Direction]
allDirs = map DAbsolute Util.listEnums <> map DRelative (DDown : map DPlanar Util.listEnums)
