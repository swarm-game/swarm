{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Metadata about achievements that the player has obtained.
module Swarm.Game.Achievement.Attainment (
  Attainment (..),
  achievement,
  maybeScenarioPath,
  obtainedAt,
) where

import Control.Lens hiding (from, (<.>))
import Data.Aeson (
  Options (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Function (on)
import Data.Time (ZonedTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Achievement.Definitions

-- | A record holding an achievement along with some metadata to
--   record the time at which the achievement was obtained, and the
--   scenario in which it was achieved.
data Attainment = Attainment
  { _achievement :: CategorizedAchievement
  -- ^ The achievement.
  , _maybeScenarioPath :: Maybe FilePath
  -- ^ From which scenario was it obtained?
  , _obtainedAt :: ZonedTime
  -- ^ What time was it obtained?
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
