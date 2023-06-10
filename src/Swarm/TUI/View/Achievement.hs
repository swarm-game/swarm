{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Achievement where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List qualified as BL
import Control.Lens ((^.))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Time.Format (defaultTimeLocale, formatTime)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Description
import Swarm.TUI.Attr
import Swarm.TUI.Model
import Swarm.TUI.Model.UI
import Text.Wrap

padAllEvenly :: Int -> Widget Name -> Widget Name
padAllEvenly x w = padTopBottom x $ padLeftRight (2 * x) w

getCompletionIcon :: Bool -> Widget Name
getCompletionIcon = \case
  False -> txt " ○  "
  True -> withAttr greenAttr $ txt " ●  "

drawAchievementsMenuUI :: AppState -> BL.List Name CategorizedAchievement -> Widget Name
drawAchievementsMenuUI s l =
  vBox
    [ hCenter $ padTopBottom 1 $ str "🏆  Achievements 🏆 "
    , hCenter $
        hBox
          [ hLimitPercent 30 $
              padAll 2 $
                BL.renderList (const $ drawAchievementListItem attainedMap) True l
          , hLimitPercent 50 $
              maybe emptyWidget (singleAchievementDetails attainedMap . snd) $
                BL.listSelectedElement l
          ]
    ]
 where
  attainedMap = s ^. uiState . uiAchievements

drawAchievementListItem ::
  Map CategorizedAchievement Attainment ->
  CategorizedAchievement ->
  Widget Name
drawAchievementListItem attainedMap x =
  getCompletionIcon wasAttained <+> titleWidget
 where
  wasAttained = M.member x attainedMap
  titleWidget = txtWrap $ title details
  details = describe x

singleAchievementDetails ::
  Map CategorizedAchievement Attainment ->
  CategorizedAchievement ->
  Widget Name
singleAchievementDetails attainedMap x =
  padRight (Pad 1) $ borderWithLabel titleWidget $ padAllEvenly 1 innerContent
 where
  wasAttained = M.member x attainedMap

  renderFlavorTextWidget :: FlavorText -> Widget Name
  renderFlavorTextWidget (Freeform t) = txtWrap t
  renderFlavorTextWidget (FTQuotation (Quotation author quoteContent)) =
    vBox
      [ txtWrap quoteContent
      , padLeft Max
          . padRight (Pad 2)
          . txtWrapWith (defaultWrapSettings {fillStrategy = FillIndent 2})
          $ "--" <> author
      ]

  innerContent =
    vBox
      [ maybe emptyWidget (padAllEvenly 2 . renderFlavorTextWidget) $ humorousElaboration details
      , txtWrap $
          if wasAttained || not (isObfuscated details)
            then attainmentProcess details
            else "???"
      , case M.lookup x attainedMap of
          Nothing -> emptyWidget
          Just attainment ->
            padTop (Pad 1) $
              vBox
                [ hBox
                    [ txt "Obtained: "
                    , withAttr cyanAttr
                        . str
                        . formatTime defaultTimeLocale "%l:%M%P on %b %e, %Y"
                        $ attainment ^. obtainedAt
                    ]
                , flip (maybe emptyWidget) (attainment ^. maybeScenarioPath) $ \s ->
                    hBox
                      [ txt "Scenario: "
                      , withAttr cyanAttr $
                          str s
                      ]
                ]
      , padTop (Pad 1) $
          hBox
            [ txt "Effort: "
            , withAttr boldAttr . str . show $ effort details
            ]
      ]

  titleWidget = padLeftRight 1 $ txt $ title details
  details = describe x
