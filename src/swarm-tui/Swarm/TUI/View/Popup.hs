{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering (& animating) notification popups.
module Swarm.TUI.View.Popup (
  drawPopups,
  startPopupAnimation,
  popupFrameDuration,
  popupFrames,
) where

import Brick (Widget (..), cropTopTo, padLeftRight, txt, vBox)
import Brick.Animation (Animation, AnimationManager, Clip, RunMode (..), newClip, renderAnimation, startAnimation)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenterLayer)
import Brick.Widgets.Core (emptyWidget, hBox, withAttr)
import Control.Lens (Traversal', (^.), (^?))
import Control.Monad.IO.Class (MonadIO)
import Swarm.Game.Achievement.Definitions (title)
import Swarm.Game.Achievement.Description (describe)
import Swarm.Game.Popup (Popup (..))
import Swarm.Language.Syntax (constInfo, syntax)
import Swarm.TUI.Model (AppEvent, AppState, animTraversal, keyConfig, keyEventHandling, playState, progression, uiPopupAnimationState, _AnimActive)
import Swarm.TUI.Model.Event qualified as SE
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Attribute.Attr (notifAttr)
import Swarm.TUI.View.Util (bindingText)
import Swarm.Util (commaList, squote)

-- | The number of milliseconds taken by each animation frame of the notification popup
popupFrameDuration :: Integer
popupFrameDuration = 50

-- | The number of animation frames for which to display a popup.
popupFrames :: Int
popupFrames = 125

-- | Draw the current notification popup (if any).
drawPopups :: AppState -> Widget Name
drawPopups s = renderAnimation (const emptyWidget) s mAnim
 where
  mAnim = s ^? playState . progression . uiPopupAnimationState . _AnimActive

-- | Signal the animation manager to start the popup animation.
startPopupAnimation :: MonadIO m => AnimationManager AppState AppEvent Name -> Popup -> m ()
startPopupAnimation mgr p = startAnimation mgr (makePopupClip p) popupFrameDuration Once trav
 where
  trav :: Traversal' AppState (Maybe (Animation AppState Name))
  trav = playState . progression . uiPopupAnimationState . animTraversal

makePopupClip :: Popup -> Clip AppState Name
makePopupClip p = newClip $ map drawPopupFrame [0 .. popupFrames]
 where
  drawPopupFrame f s = hCenterLayer $ cropTopTo (popupRows f) . border . padLeftRight 2 $ drawPopup s p

drawPopup :: AppState -> Popup -> Widget Name
drawPopup s = \case
  AchievementPopup ach ->
    hBox
      [ withAttr notifAttr (txt "Achievement unlocked: ")
      , txt (title (describe ach))
      ]
  RecipesPopup ->
    hBox
      [ withAttr notifAttr (txt "New recipes unlocked! ")
      , txt $ bindingText keyConf (SE.Main SE.ViewRecipesEvent) <> " to view."
      ]
  CommandsPopup cmds ->
    vBox
      [ hBox
          [ withAttr notifAttr (txt "New commands unlocked: ")
          , txt . commaList $ map (squote . syntax . constInfo) cmds
          ]
      , txt $ "Hit " <> bindingText keyConf (SE.Main SE.ViewCommandsEvent) <> " to view all available commands."
      ]
  DebugWarningPopup ->
    hBox
      [ withAttr notifAttr (txt "Warning: ")
      , txt "No progress will be saved, since debugging flags are in use."
      ]
 where
  keyConf = s ^. keyEventHandling . keyConfig

-- | Compute the number of rows of the notification popup we should be
--   showing, based on the number of animation frames the popup has existed.
--   This is what causes the popup to animate in and out of existence.
popupRows :: Int -> Int
popupRows f
  -- If we're less than halfway through the lifetime of the popup, use
  -- the number of animation frames elapsed since the beginning of the popup animation.
  -- This will become much larger than the actual number of rows in the
  -- popup, but the 'cropTopTo' function simply has no effect when given any value
  -- equal to or larger than the number of rows of a widget. This way the animation
  -- will continue to work for popups with any (reasonable) number of rows.
  | f <= popupFrames `div` 2 = f
  -- Otherwise, use the number of frames remaining.
  | otherwise = popupFrames - f
