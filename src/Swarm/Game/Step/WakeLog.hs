-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Step.WakeLog where

import Data.Aeson (Options (..), SumEncoding (ObjectWithSingleField), ToJSON (..), defaultOptions, genericToJSON)
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.CESK (TickNumber)
import Swarm.Game.Robot (RID)

data WakeLogEvent = WakeLogEvent
  { targetRobotID :: RID
  , thisEventTime :: TickNumber
  , thisEventType :: WakeLogEventType
  }
  deriving (Show, Eq, Generic, ToJSON)

data WakeLogEventType
  = ScheduledWakeup
  | CalledWaitCommand
  | CalledSwapCommand
  | DoneSleeping
  deriving (Show, Eq, Generic, ToJSON)

-- instance ToJSON WakeLogEventType where
--   toJSON = genericToJSON $ defaultOptions
--     { sumEncoding = ObjectWithSingleField
--     }

instance ToSample WakeLogEvent where
  toSamples _ = SD.noSamples
