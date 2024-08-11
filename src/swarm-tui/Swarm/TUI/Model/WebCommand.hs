-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The type of commands sent from Web API handlers to the Controller,
-- and the type of replies.
module Swarm.TUI.Model.WebCommand (
  WebCommand (..),
  WebInvocationState (..),
  RejectionReason (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Util.JSON (optionsMinimize)

data WebCommand = RunWebCode {webEntry :: Text, webReply :: WebInvocationState -> IO ()}

data RejectionReason = NoActiveGame | AlreadyRunning | ParseError String
  deriving (Eq, Ord, Show, Generic)

data WebInvocationState = Rejected RejectionReason | InProgress | Complete String
  deriving (Eq, Ord, Show, Generic)

-- --------------------------
-- ToJSON/FromJSON Instances
-- --------------------------

instance ToJSON RejectionReason where
  toJSON = genericToJSON optionsMinimize

instance FromJSON RejectionReason where
  parseJSON = genericParseJSON optionsMinimize

instance ToJSON WebInvocationState where
  toJSON = genericToJSON optionsMinimize

instance FromJSON WebInvocationState where
  parseJSON = genericParseJSON optionsMinimize
