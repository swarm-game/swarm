{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Values where

import Control.Lens (view)
import Data.Int (Int32)
import Linear (V2 (..))
import Swarm.Game.Entity
import Swarm.Game.Robot
import Swarm.Language.Value

class Valuable a where
  asValue :: a -> Value

instance Valuable Int32 where
  asValue = VInt . fromIntegral

instance (Valuable a) => Valuable (V2 a) where
  asValue (V2 x y) = VPair (asValue x) (asValue y)

instance Valuable Entity where
  asValue = VText . view entityName

instance Valuable Robot where
  asValue = VRobot . view robotID

instance (Valuable a) => Valuable (Maybe a) where
  asValue Nothing = VInj False VUnit
  asValue (Just x) = VInj True $ asValue x

instance (Valuable a, Valuable b) => Valuable (Either a b) where
  asValue (Left x) = VInj False $ asValue x
  asValue (Right x) = VInj True $ asValue x
