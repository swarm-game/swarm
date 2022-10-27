{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.View.Achievement where

-- import Control.Lens ((^.))
import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Swarm.TUI.Attr
import Swarm.TUI.Model.Names
import Brick.Widgets.List qualified as BL
import Swarm.TUI.Model.Achievement.Definitions
import Swarm.TUI.Model
import Brick.Widgets.Center (hCenter)

padAllEvenly :: Int -> Widget Name -> Widget Name
padAllEvenly x w = padTopBottom x $ padLeftRight (2 * x) w

getCompletionIcon :: Bool -> Widget Name
getCompletionIcon = \case
  False -> txt " ○  "
  True -> withAttr greenAttr $ txt " ●  "

drawAchievementsMenuUI :: AppState -> BL.List Name CategorizedAchievement -> Widget Name
drawAchievementsMenuUI _s l =
  vBox [
    hCenter $ padTopBottom 1 $ str "🏆 Achievements 🏆"
  , hBox [
      hLimitPercent 50 $ padAll 2 $
        BL.renderList (const drawAchievementListItem) True l
    , maybe emptyWidget (singleAchievementDetails . snd) $ BL.listSelectedElement l
    ]
  ]
 where
  drawAchievementListItem :: CategorizedAchievement -> Widget Name
  drawAchievementListItem x =
    getCompletionIcon False <+> titleWidget
    where
      titleWidget = txt $ title details
      details = describe x


singleAchievementDetails :: CategorizedAchievement -> Widget Name
singleAchievementDetails x =
  borderWithLabel titleWidget $ padAllEvenly 1 innerContent
  where
    renderFlavorTextWidget :: FlavorText -> Widget Name
    renderFlavorTextWidget (Freeform t) = txt t
    renderFlavorTextWidget (FTQuotation (Quotation author quoteContent)) =
      vBox [
          txt quoteContent
        , padLeft Max $ padRight (Pad 2) $ txt $ "--" <> author
        ]

    innerContent = vBox [
        maybe emptyWidget (padAllEvenly 1 . border . padAll 2 . renderFlavorTextWidget) $ humorousElaboration details
      , txt "foo"
      ]

    titleWidget = padLeftRight 1 $ txt $ title details
    details = describe x