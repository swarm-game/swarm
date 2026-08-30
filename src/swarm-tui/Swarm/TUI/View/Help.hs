{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the help system.
module Swarm.TUI.View.Help (drawHelpUI) where

import Brick
import Brick.Keybindings (KeyConfig)
import Brick.Widgets.Center (hCenter)
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Swarm.Game.State.Runtime (helpData)
import Swarm.Language.Help (HelpPage, helpDoc, helpMetadata)
import Swarm.Language.Syntax (Phase (Raw), Syntax)
import Swarm.ResourceLoading (atPath)
import Swarm.TUI.Border (BorderLabels, borderWithLabels, bottomLabels, centerLabel, leftLabel, plainBorder, plainHBorder, topLabels)
import Swarm.TUI.Model (AppState, Name (..), keyConfig, keyEventHandling, runtimeState)
import Swarm.TUI.Model.Event (MainEvent (HelpBackEvent, HelpFwdEvent), SwarmEvent (Main))
import Swarm.TUI.View.KeyCmd
import Swarm.TUI.View.Util (bindingText, drawMarkdown)
import Swarm.Text.Markdown (Document, toText)

drawHelpUI :: AppState -> FilePath -> [Widget Name]
drawHelpUI s hp = [helpPageWidget hp (help ^? atPath hp) keyConf]
 where
  help = s ^. runtimeState . helpData
  keyConf = s ^. keyEventHandling . keyConfig

helpPageWidget :: FilePath -> Maybe HelpPage -> KeyConfig SwarmEvent -> Widget Name
helpPageWidget path mhp keyConf =
  borderWithLabels labels
    . withVScrollBars OnRight
    . viewport HelpViewport Vertical
    -- Pad 1 on the right first, before padRight Max, to make sure
    -- there is always at least one space of padding
    . padRight Max
    . padRight (Pad 1)
    . padLeft (Pad 1)
    . padTop (Pad 1)
    $ content
 where
  labels :: BorderLabels Name
  labels =
    plainBorder
      & topLabels
        .~ ( plainHBorder
               & centerLabel ?~ txt (toText title)
           )
      & bottomLabels
        .~ ( plainHBorder
               & leftLabel ?~ drawKeyCmds helpCmds
           )

  title :: Document (Syntax Raw)
  title = case mhp of
    Nothing -> "Page not found"
    Just hp -> fromMaybe "Untitled" (hp ^. helpMetadata . at "title")

  helpCmds :: [KeyCmd]
  helpCmds =
    [ SingleButton NoHighlight (bindingText keyConf $ Main HelpBackEvent) "back"
    , SingleButton NoHighlight (bindingText keyConf $ Main HelpFwdEvent) "forward"
    , SingleButton NoHighlight "Esc" "exit"
    ]

  content :: Widget Name
  content = case mhp of
    Nothing -> padTop (Pad 2) . hCenter . txt $ "No help page exists at path " <> T.pack path
    Just hp -> drawMarkdown (hp ^. helpDoc)
