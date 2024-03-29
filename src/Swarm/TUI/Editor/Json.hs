-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Editor.Json where

import Data.Text (Text)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Language.Syntax (Syntax)
import Swarm.Language.Text.Markdown (Document)

data SkeletonScenario = SkeletonScenario
  { version :: Int
  , name :: Text
  , description :: Document Syntax
  , creative :: Bool
  , entities :: [Entity]
  , world :: WorldDescriptionPaint
  , robots :: [String]
  }
  deriving (Generic)

instance ToJSON SkeletonScenario
