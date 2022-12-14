{-# LANGUAGE TemplateHaskell #-}

module Swarm.TUI.Model.Achievement.Attainment where

import Control.Lens hiding (from, (<.>))
import Data.Aeson (
  Options (..),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
 )
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.TUI.Model.Achievement.Definitions

data Attainment = Attainment
  { _achievement :: CategorizedAchievement
  , _maybeScenarioPath :: Maybe Text
  -- ^ which scenario was it obtained?
  , _obtainedAt :: ZonedTime
  }
  deriving (Generic)

makeLenses ''Attainment

instance FromJSON Attainment where
  parseJSON = genericParseJSON achievementJsonOptions

instance ToJSON Attainment where
  toEncoding = genericToEncoding achievementJsonOptions
  toJSON = genericToJSON achievementJsonOptions

achievementJsonOptions :: Options
achievementJsonOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length "_")
    }
