{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Model.Achievement.Definitions where

import Data.Text (Text)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Util

data ExpectedEffort
  = Trivial
  | Easy
  | Moderate
  | Gruelling
  deriving (Eq, Ord, Show, Bounded, Enum)

data Quotation = Quotation
  { attribution :: Text
  , content :: Text
  }
  deriving (Show)

data FlavorText
  = Freeform Text
  | FTQuotation Quotation
  deriving (Show)

data AchievementInfo = AchievementInfo
  { title :: Text
  -- ^ Guidelines:
  -- * prefer puns, pop culture references, etc.
  -- * should be a phrase in Title Case.
  -- * For achievements that are "hidden", this can be
  --   a vague "clue" as to what the attainment entails.
  , humorousElaboration :: Maybe FlavorText
  -- ^ Explain the reference, e.g. in the form of a full quote
  -- from a movie, or something you might find
  -- in a fortune cookie
  , attainmentProcess :: Text
  -- ^ Precisely what must be done to obtain this achievement.
  , difficulty :: ExpectedEffort
  , isHidden :: Bool
  }
  deriving (Show)

data CategorizedAchievement
  = GlobalAchievement GlobalAchievement
  | GameplayAchievement GameplayAchievement
  deriving (Eq, Ord, Show, Generic)

instance FromJSON CategorizedAchievement
instance ToJSON CategorizedAchievement

-- | Achievements that entail some aggregate of actions
-- across scenarios
data GlobalAchievement
  = CompletedTutorials
  | LookedAtAboutScreen
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON GlobalAchievement
instance ToJSON GlobalAchievement

-- | Achievements obtained while playing a single scenario
data GameplayAchievement
  = RobotIntoWater
  | DestroyedBase
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance FromJSON GameplayAchievement
instance ToJSON GameplayAchievement

listAchievements :: [CategorizedAchievement]
listAchievements =
  map GlobalAchievement listEnums
    <> map GameplayAchievement listEnums

describe :: CategorizedAchievement -> AchievementInfo
describe (GlobalAchievement CompletedTutorials) =
  AchievementInfo
    "Autodidact"
    ( Just $
        FTQuotation $
          Quotation
            "Terry Pratchet"
            "I didn't go to university... But I have sympathy for those who did."
    )
    "Complete all of the tutorials."
    Moderate
    False
describe (GlobalAchievement LookedAtAboutScreen) =
  AchievementInfo
    "About time!"
    Nothing
    "View the About screen."
    Trivial
    True
describe (GameplayAchievement RobotIntoWater) =
  AchievementInfo
    "Watery Grave"
    (Just $ Freeform "This little robot thinks he's a submarine.")
    "Destroy a robot by sending it into the water."
    Easy
    True
describe (GameplayAchievement DestroyedBase) =
  AchievementInfo
    "That Could Have Gone Better"
    (Just $ Freeform "Boom.")
    "Destroy your base."
    Easy
    True
