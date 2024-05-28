{-# LANGUAGE DeriveGeneric #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Tick (
  TickNumber (..),
  addTicks,
) where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as A
import Data.Int (Int64)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Swarm.Util.JSON (optionsUnwrapUnary)
import Swarm.Util.WindowedCounter (Offsettable (..))

-- | A newtype representing a count of ticks (typically since the
--   start of a game).
newtype TickNumber = TickNumber {getTickNumber :: Int64}
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON TickNumber where
  toJSON = A.genericToJSON optionsUnwrapUnary

instance FromJSON TickNumber where
  parseJSON = A.genericParseJSON optionsUnwrapUnary

-- | Add an offset to a 'TickNumber'.
addTicks :: Int -> TickNumber -> TickNumber
addTicks i (TickNumber n) = TickNumber $ n + fromIntegral i

instance Offsettable TickNumber where
  offsetBy = addTicks

instance Pretty TickNumber where
  pretty (TickNumber i) = pretty i
