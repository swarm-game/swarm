module Swarm.Game.Achievement.Definitions where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Util

data ExpectedEffort
  = Trivial
  | Easy
  | Moderate
  | Gruelling
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, FromJSON, ToJSON)

data Quotation = Quotation
  { attribution :: Text
  , content :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data FlavorText
  = Freeform Text
  | FTQuotation Quotation
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AchievementInfo = AchievementInfo
  { title :: Text
  -- ^ Guidelines:
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
  , isObfuscated :: Bool
  -- ^ Hides the attainment process until after the achievement is attained.
  -- Best when the title + elaboration constitute a good clue.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

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

-- | Achievements that entail some aggregate of actions
-- across scenarios
data GlobalAchievement
  = CompletedSingleTutorial
  | CompletedAllTutorials
  | LookedAtAboutScreen
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON GlobalAchievement
instance ToJSON GlobalAchievement

-- | Achievements obtained while playing a single scenario
data GameplayAchievement
  = CraftedBitcoin
  | RobotIntoWater
  | AttemptSelfDestructBase
  | DestroyedBase
  | LoseScenario
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON GameplayAchievement
instance ToJSON GameplayAchievement

listAchievements :: [CategorizedAchievement]
listAchievements =
  map GlobalAchievement listEnums
    <> map GameplayAchievement listEnums
