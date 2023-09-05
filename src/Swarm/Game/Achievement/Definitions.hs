-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of all possible achievements.
module Swarm.Game.Achievement.Definitions (
  -- * Achievements
  CategorizedAchievement (..),
  GlobalAchievement (..),
  GameplayAchievement (..),
  listAchievements,

  -- * Achievement info
  ExpectedEffort (..),
  Quotation (..),
  FlavorText (..),
  AchievementInfo (..),
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Util

-- | How hard do we expect the achievement to be?
data ExpectedEffort
  = Trivial
  | Easy
  | Moderate
  | Gruelling
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, FromJSON, ToJSON)

-- | A quotation to spice up the description of an achievement.
data Quotation = Quotation
  { attribution :: Text
  , content :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Flavor text to spice up the description of an achievement, either
--   freeform text or a quotation.
data FlavorText
  = Freeform Text
  | FTQuotation Quotation
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Information about an achievement.  See
--   "Swarm.Game.Achievement.Description" for a mapping from
--   achievements to an corresponding 'AchievementInfo' record.
data AchievementInfo = AchievementInfo
  { title :: Text
  -- ^ Guidelines:
  --
  -- * prefer puns, pop culture references, etc.
  -- * should be a phrase in Title Case.
  -- * For achievements that are "obfuscated", this can be
  --   a vague "clue" as to what the attainment entails.
  , humorousElaboration :: Maybe FlavorText
  -- ^ Explain the reference, e.g. in the form of a full quote
  -- from a movie, or something you might find
  -- in a fortune cookie
  , attainmentProcess :: Text
  -- ^ Precisely what must be done to obtain this achievement.
  , effort :: ExpectedEffort
  -- ^ How hard the achievement is expected to be.
  , isObfuscated :: Bool
  -- ^ Hides the attainment process until after the achievement is attained.
  --   Best when the title + elaboration constitute a good clue.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | An achievement, categorized as either global or gameplay.
data CategorizedAchievement
  = GlobalAchievement GlobalAchievement
  | GameplayAchievement GameplayAchievement
  deriving (Eq, Ord, Show, Generic)

categorizedAchievementJsonOptions :: Options
categorizedAchievementJsonOptions =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

instance ToJSON CategorizedAchievement where
  toJSON = genericToJSON categorizedAchievementJsonOptions

instance FromJSON CategorizedAchievement where
  parseJSON = genericParseJSON categorizedAchievementJsonOptions

-- | Achievements that entail some aggregate of actions across
--   scenarios, or are independent of any particular scenario.
data GlobalAchievement
  = CompletedSingleTutorial
  | CompletedAllTutorials
  | LookedAtAboutScreen
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON GlobalAchievement
instance ToJSON GlobalAchievement

-- | Achievements obtained while playing a single scenario.
data GameplayAchievement
  = CraftedBitcoin
  | RobotIntoWater
  | AttemptSelfDestructBase
  | DestroyedBase
  | LoseScenario
  | GetDisoriented
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON GameplayAchievement
instance ToJSON GameplayAchievement

-- | List of all possible achievements.
listAchievements :: [CategorizedAchievement]
listAchievements =
  map GlobalAchievement listEnums
    <> map GameplayAchievement listEnums
