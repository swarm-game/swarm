{-# LANGUAGE DeriveGeneric #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Tick (
  TickNumber (..),
  addTicks,
  formatTicks,
) where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as A
import Data.Bits (Bits (..))
import Data.Int (Int64)
import Data.List (intersperse)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Swarm.Util.JSON (optionsUnwrapUnary)
import Swarm.Util.WindowedCounter (Offsettable (..))
import Text.Printf

-- | A newtype representing a count of ticks (typically since the
--   start of a game).
newtype TickNumber = TickNumber {getTickNumber :: Int64}
  deriving (Eq, Ord, Show, Read, Generic)

-- | Add an offset to a 'TickNumber'.
addTicks :: Int -> TickNumber -> TickNumber
addTicks i (TickNumber n) = TickNumber $ n + fromIntegral i

-- | Format a ticks count as a hexadecimal clock.
formatTicks :: Bool -> TickNumber -> String
formatTicks showTicks (TickNumber t) =
  mconcat $
    intersperse
      ":"
      [ printf "%x" (t `shiftR` 20)
      , printf "%02x" ((t `shiftR` 12) .&. ((1 `shiftL` 8) - 1))
      , printf "%02x" ((t `shiftR` 4) .&. ((1 `shiftL` 8) - 1))
      ]
      ++ if showTicks then [".", printf "%x" (t .&. ((1 `shiftL` 4) - 1))] else []

instance ToJSON TickNumber where
  toJSON = A.genericToJSON optionsUnwrapUnary

instance FromJSON TickNumber where
  parseJSON = A.genericParseJSON optionsUnwrapUnary

instance Offsettable TickNumber where
  offsetBy = addTicks

instance Pretty TickNumber where
  pretty = pretty . formatTicks True
