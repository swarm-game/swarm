{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent in-game logs by robots.
--
-- Because of the use of system robots, we sometimes
-- want to use special kinds of logs that will be
-- shown to the player.
--
-- TODO: #1039 Currently we abuse this system for system
-- logs, which is fun, but we should eventually make
-- a dedicated `SystemLogEntry` type for 'RuntimeState'
-- message queue.
module Swarm.Game.Log (
  LogSource (..),
  ErrorLevel (..),

  -- * Robot log entries
  LogEntry (..),
  leText,
  leSource,
  leRobotName,
  leTime,
  leLocation,
  leRobotID,
) where

import Control.Lens hiding (contains)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Game.CESK (TickNumber)
import Swarm.Game.Location (Location)

-- | Severity of the error - critical errors are bugs
--   and should be reported as Issues.
data ErrorLevel = Debug | Warning | Error | Critical
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Source of the robot log.
data LogSource
  = -- | Log produced by 'Swarm.Language.Syntax.Say'
    Said
  | -- | Log produced by 'Swarm.Language.Syntax.Log'
    Logged
  | -- | Log produced by an exception or system.
    ErrorTrace ErrorLevel
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | An entry in a robot's log.
data LogEntry = LogEntry
  { _leTime :: TickNumber
  -- ^ The time at which the entry was created.
  --   Note that this is the first field we sort on.
  , _leSource :: LogSource
  -- ^ Whether this log records a said message.
  , _leRobotName :: Text
  -- ^ The name of the robot that generated the entry.
  , _leRobotID :: Int
  -- ^ The ID of the robot that generated the entry.
  , _leLocation :: Location
  -- ^ Location of the robot at log entry creation.
  , _leText :: Text
  -- ^ The text of the log entry.
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

makeLenses ''LogEntry
