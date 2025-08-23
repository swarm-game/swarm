{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Flavor text
--
-- Flavor text about all defined achievements.
module Swarm.Game.Achievement.Description where

import Swarm.Game.Achievement.Definitions

-- | Function mapping each 'CategorizedAchievement' to an appropriate
--   'AchievementInfo' record.  This function must be updated whenever
--   a new type of achievement is added.
describe :: CategorizedAchievement -> AchievementInfo
describe = \case
  GlobalAchievement CompletedSingleTutorial ->
    AchievementInfo
      "Welcome Freshmen"
      (Just $ Freeform "School is in session!")
      "Complete one of the tutorials."
      Easy
      False
  GlobalAchievement CompletedAllTutorials ->
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
  GlobalAchievement LookedAtAboutScreen ->
    AchievementInfo
      "About time!"
      Nothing
      "View the About screen."
      Trivial
      True
  GameplayAchievement CraftedBitcoin ->
    -- Bitcoin is the deepest level of the recipes
    -- hierarchy.
    AchievementInfo
      "Master of Your Craft"
      Nothing
      "Make a Bitcoin"
      Moderate
      True
  GameplayAchievement RobotIntoWater ->
    AchievementInfo
      "Watery Grave"
      (Just $ Freeform "This little robot thinks he's a submarine.")
      "Destroy a robot by sending it into the water."
      Easy
      True
  GameplayAchievement AttemptSelfDestructBase ->
    AchievementInfo
      "Call of the Void"
      (Just $ Freeform "What does that big red button do?")
      "Attempt to self-destruct your base."
      Easy
      True
  GameplayAchievement DestroyedBase ->
    AchievementInfo
      "That Could Have Gone Better"
      (Just $ Freeform "Boom.")
      "Actually destroy your base."
      Moderate
      True
  GameplayAchievement LoseScenario ->
    AchievementInfo
      "Silver Lining"
      (Just $ Freeform "Here's your consolation prize.")
      "Lose at a scenario."
      Easy
      True
  GameplayAchievement GetDisoriented ->
    AchievementInfo
      "Playing Ostrich"
      ( Just $
          FTQuotation $
            Quotation
              "Lil Jon"
              "Fire up that loud / Another round of shots / Turn down for what?"
      )
      "`turn down` without a compass. Congratulations, you are \"disoriented\". How are you supposed to move now?"
      Easy
      True
  GameplayAchievement SwapSame ->
    AchievementInfo
      "Fair Trade"
      (Just $ Freeform "The *Law of Equivalent Exchange*... taken literally.")
      "`swap` an item for itself."
      Easy
      True
  GameplayAchievement GaveToSelf ->
    AchievementInfo
      "Treat. Yo. Self."
      (Just $ FTQuotation $ Quotation "Tom, Parks and Recreation" "Fragrances. Massages. Treat yourself. Mimosas. Fine leather goods...It's the best day of the year.")
      "`give` something to your`self`."
      Easy
      True
  GameplayAchievement EquippedAllDevices ->
    AchievementInfo
      "Swiss Army Robot"
      (Just $ Freeform "You never know when that might come in handy...")
      "`equip` all craftable devices simultaneously."
      Gruelling
      True
  GameplayAchievement UnequippedWelder ->
    AchievementInfo
      "Put That Back!"
      (Just $ Freeform "Maybe... you should have thought that one through first.")
      "`unequip` a welder."
      Easy
      True

-- | Validity conditions are required if-and-only-if the achievement
-- category is 'GameplayAchievement'.
getValidityRequirements :: GameplayAchievement -> ValidityConditions
getValidityRequirements = \case
  CraftedBitcoin -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  RobotIntoWater -> ValidityConditions OnlyPlayerRobot ValidInCreativeMode
  AttemptSelfDestructBase -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  DestroyedBase -> ValidityConditions OnlyPlayerRobot ValidInCreativeMode
  LoseScenario -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  GetDisoriented -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  SwapSame -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  GaveToSelf -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  EquippedAllDevices -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
  UnequippedWelder -> ValidityConditions OnlyPlayerRobot ExcludesCreativeMode
