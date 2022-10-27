{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Model.Achievement.Description where

import Swarm.TUI.Model.Achievement.Definitions

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
