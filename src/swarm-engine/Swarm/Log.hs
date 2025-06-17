{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent log messages, both for robot logs and
-- the system log.
module Swarm.Log (
  -- * Log entries
  Severity (..),
  RobotLogSource (..),
  LogSource (..),
  LogEntry (..),
  leTime,
  leSource,
  leSeverity,
  leName,
  leText,

  -- * Utilities
  logToText,
) where

import Control.Lens (makeLenses, over, view, _head)
import Data.Aeson
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Docs qualified as SD
import Swarm.Game.Location (Location)
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Game.Universe (Cosmic)
import Swarm.Util.JSON (optionsMinimize, optionsUntagged)

-- | Severity of the error - critical errors are bugs
--   and should be reported as Issues.
data Severity = Debug | Info | Warning | Error | Critical
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Severity where
  toJSON = genericToJSON optionsUntagged

instance FromJSON Severity where
  parseJSON = genericParseJSON optionsUntagged

-- | How a robot log entry was produced.
data RobotLogSource
  = -- | Produced by 'Swarm.Language.Syntax.Say'
    Said
  | -- | Produced by 'Swarm.Language.Syntax.Log'
    Logged
  | -- | Produced as the result of an error.
    RobotError
  | -- | Produced as a status message from a command.
    CmdStatus
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RobotLogSource where
  toJSON = genericToJSON optionsUntagged

instance FromJSON RobotLogSource where
  parseJSON = genericParseJSON optionsUntagged

-- | Source of a log entry.
data LogSource
  = -- | Log produced by a robot.  Stores information about which
    --   command was used and the ID and location of the producing
    --   robot.
    RobotLog RobotLogSource Int (Cosmic Location)
  | -- | Log produced by an exception or system.
    SystemLog
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LogSource where
  toJSON = genericToJSON optionsUntagged

instance FromJSON LogSource where
  -- This is not ambiguos for Robot/System constructor, but
  -- read aeson docs before adding new LogSource constructor
  parseJSON = genericParseJSON optionsUntagged

-- | A log entry.
data LogEntry = LogEntry
  { _leTime :: TickNumber
  -- ^ The time at which the entry was created.
  --   Note that this is the first field we sort on.
  , _leSource :: LogSource
  -- ^ Where this log message came from.
  , _leSeverity :: Severity
  -- ^ Severity level of this log message.
  , _leName :: Text
  -- ^ Name of the robot or subsystem that generated this log entry.
  , _leText :: Text
  -- ^ The text of the log entry.
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''LogEntry

-- | Extract the text from a container of log entries.
logToText :: Foldable t => t LogEntry -> [Text]
logToText = map (view leText) . toList

instance FromJSON LogEntry where
  parseJSON = genericParseJSON entryJsonOptions

instance ToJSON LogEntry where
  toJSON = genericToJSON entryJsonOptions

entryJsonOptions :: Options
entryJsonOptions =
  optionsMinimize
    { fieldLabelModifier = over _head toLower . drop 3 -- drops prefix
    }

instance SD.ToSample LogEntry where
  toSamples _ =
    SD.singleSample $
      LogEntry
        { _leTime = TickNumber 0
        , _leSource = SystemLog
        , _leSeverity = Warning
        , _leName = "Loading game"
        , _leText = "Can not open file XYZ!"
        }
