module Swarm.TUI.Editor.Json where

import Data.Text (Text)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.WorldDescription

data SkeletonScenario = SkeletonScenario
  { version :: Int
  , name :: Text
  , description :: Text
  , creative :: Bool
  , entities :: [Entity]
  , world :: WorldDescriptionPaint
  , robots :: [String]
  }
  deriving (Generic)

instance ToJSON SkeletonScenario
