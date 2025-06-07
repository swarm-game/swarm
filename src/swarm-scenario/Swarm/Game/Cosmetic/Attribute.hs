{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract attributes for coloring terrain, and entities, and
-- robots. (But NOT TUI elements --- for that, see
-- 'Swarm.TUI.View.Attribute.Attr'.)
module Swarm.Game.Cosmetic.Attribute where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Yaml

-- | An internal attribute name.
data Attribute = ARobot | AEntity | AWorld Text
  deriving (Eq, Ord, Show, Generic, Hashable)

readAttribute :: Text -> Attribute
readAttribute = \case
  "robot" -> ARobot
  "entity" -> AEntity
  w -> AWorld w

instance FromJSON Attribute where
  parseJSON = withText "attribute" $ pure . readAttribute

instance ToJSON Attribute where
  toJSON = \case
    ARobot -> String "robot"
    AEntity -> String "entity"
    AWorld w -> String w
