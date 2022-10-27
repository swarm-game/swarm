{-# LANGUAGE TemplateHaskell #-}

module Swarm.TUI.Model.Achievement.Attainment where

import Control.Lens hiding (from, (<.>))
import Data.Aeson (
  Options (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Function (on)
import Data.Text (Text)
import Data.Time (ZonedTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.TUI.Model.Achievement.Definitions

data Attainment = Attainment
  { _achievement :: CategorizedAchievement
  , _maybeScenarioPath :: Maybe Text
  -- ^ from which scenario was it obtained?
  , _obtainedAt :: ZonedTime
  }
  deriving (Generic)

makeLenses ''Attainment

instance Eq Attainment where
  (==) = (==) `on` _achievement

instance Ord Attainment where
  compare = compare `on` (zonedTimeToUTC . _obtainedAt)

instance Semigroup Attainment where
  (<>) = min

instance FromJSON Attainment where
  parseJSON = genericParseJSON achievementJsonOptions

instance ToJSON Attainment where
  toJSON = genericToJSON achievementJsonOptions

achievementJsonOptions :: Options
achievementJsonOptions =
  defaultOptions
    { fieldLabelModifier = tail -- drops leading underscore
    }
