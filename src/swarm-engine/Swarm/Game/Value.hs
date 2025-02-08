{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Conversions from native Haskell values
-- to values in the swarm language.
module Swarm.Game.Value where

import Control.Lens (view)
import Data.Either.Extra (maybeToEither)
import Data.Int (Int32)
import Data.List (uncons)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Linear (V2 (..))
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..))
import Swarm.Game.Universe
import Swarm.Language.Syntax.Direction
import Swarm.Language.Value

-- * Patterns

type VRect = Value
pattern VRect :: Integer -> Integer -> Integer -> Integer -> VRect
pattern VRect x1 y1 x2 y2 = VPair (VPair (VInt x1) (VInt y1)) (VPair (VInt x2) (VInt y2))

-- * Conversions

-- | Conversion from native Haskell types
-- to their swarm-lang equivalents, useful for
-- implementing swarm
-- <https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet commands>
-- in Haskell.
class Valuable a where
  asValue :: a -> Value

instance Valuable Value where
  asValue = id

instance Valuable Int32 where
  asValue = VInt . fromIntegral

instance Valuable Int where
  asValue = VInt . fromIntegral

instance Valuable Integer where
  asValue = VInt

instance Valuable Bool where
  asValue = VBool

instance Valuable Text where
  asValue = VText

instance Valuable SubworldName where
  asValue = asValue . renderWorldName

instance Valuable () where
  asValue = const VUnit

instance (Valuable a) => Valuable (V2 a) where
  asValue (V2 x y) = asValue (x, y)

instance (Valuable a, Valuable b) => Valuable (a, b) where
  asValue (x, y) = VPair (asValue x) (asValue y)

instance Valuable Location where
  asValue (Location x y) = asValue (x, y)

instance Valuable Entity where
  asValue = VText . view entityName

instance Valuable Robot where
  asValue = VRobot . view robotID

instance Valuable Direction where
  asValue = VDir

instance (Valuable a) => Valuable (Maybe a) where
  asValue = asValue . maybeToEither ()

instance (Valuable a, Valuable b) => Valuable (Either a b) where
  asValue (Left x) = VInj False $ asValue x
  asValue (Right x) = VInj True $ asValue x

instance Valuable a => Valuable [a] where
  asValue = asValue . uncons

instance Valuable a => Valuable (NE.NonEmpty a) where
  asValue = asValue . NE.toList

instance Valuable AreaDimensions where
  asValue (AreaDimensions w h) = asValue (w, h)
