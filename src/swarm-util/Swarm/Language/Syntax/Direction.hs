{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types and helper functions for working with directions
module Swarm.Language.Syntax.Direction (
  -- * Directions
  Direction (..),
  AbsoluteDir (..),
  RelativeDir (..),
  PlanarRelativeDir (..),
  directionSyntax,
  isCardinal,
  allDirs,
  directionJsonModifier,
  getCoordinateOrientation,
) where

import Data.Aeson.Types hiding (Key)
import Data.Char qualified as C (toLower)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.List qualified as L (drop)
import Data.List.Extra (enumerate)
import Data.Text hiding (filter, length, map)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter (pretty)
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Util.JSON (optionsMinimize)

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
-- (e.g. 'Swarm.Game.Location.nearestDirection' and 'Swarm.Game.Location.relativeTo').
data AbsoluteDir = DEast | DNorth | DWest | DSouth
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, Enum, Bounded)

directionJsonModifier :: String -> String
directionJsonModifier = map C.toLower . L.drop 1

data CoordinateOrientation
  = Latitudinal
  | Longitudinal
  deriving (Show, Eq, Ord)

getCoordinateOrientation :: AbsoluteDir -> CoordinateOrientation
getCoordinateOrientation = \case
  DEast -> Longitudinal
  DWest -> Longitudinal
  DNorth -> Latitudinal
  DSouth -> Latitudinal

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
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable)

instance ToJSON RelativeDir where
  toJSON = genericToJSON optionsMinimize

instance FromJSON RelativeDir where
  parseJSON = genericParseJSON optionsMinimize

-- | Caution: Do not alter this ordering, as there exist functions that depend on it
-- (e.g. 'Swarm.Game.Location.nearestDirection' and 'Swarm.Game.Location.relativeTo').
data PlanarRelativeDir = DForward | DLeft | DBack | DRight
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable, Enum, Bounded)

instance FromJSON PlanarRelativeDir where
  parseJSON = genericParseJSON directionJsonOptions

instance ToJSON PlanarRelativeDir where
  toJSON = genericToJSON directionJsonOptions

-- | The type of directions. Used /e.g./ to indicate which way a robot
--   will turn.
data Direction = DAbsolute AbsoluteDir | DRelative RelativeDir
  deriving (Eq, Ord, Show, Read, Generic, Data, Hashable)

instance FromJSON Direction where
  parseJSON = genericParseJSON optionsMinimize

instance ToJSON Direction where
  toJSON = genericToJSON optionsMinimize

-- | Direction name is generated from the deepest nested data constructor
-- e.g. 'DLeft' becomes "left"
directionSyntax :: Direction -> Text
directionSyntax d = T.pack $ directionJsonModifier $ case d of
  DAbsolute x -> show x
  DRelative x -> case x of
    DPlanar y -> show y
    _ -> show x

instance PrettyPrec Direction where
  prettyPrec _ = pretty . directionSyntax

-- | Check if the direction is absolute (e.g. 'Swarm.Game.Location.north' or 'Swarm.Game.Location.south').
isCardinal :: Direction -> Bool
isCardinal = \case
  DAbsolute _ -> True
  _ -> False

allDirs :: [Direction]
allDirs = map DAbsolute enumerate <> map DRelative (DDown : map DPlanar enumerate)
