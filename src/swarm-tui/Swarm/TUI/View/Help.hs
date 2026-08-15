{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the help system.
module Swarm.TUI.View.Help (drawHelpUI) where

import Brick
import Brick.Widgets.Center (hCenter)
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Swarm.Language.Help (HelpPage, helpDoc, helpMetadata)
import Swarm.Language.Syntax (Phase (Raw), Syntax)
import Swarm.ResourceLoading (Collection, atPath)
import Swarm.TUI.Border (BorderLabels, borderWithLabels, centerLabel, plainBorder, plainHBorder, topLabels)
import Swarm.TUI.Model (Name (..))
import Swarm.TUI.View.Util (drawMarkdown)
import Swarm.Text.Markdown (Document, toText)

drawHelpUI :: Collection HelpPage -> FilePath -> [Widget Name]
drawHelpUI help hp = [helpPageWidget hp (help ^? atPath hp)]

helpPageWidget :: FilePath -> Maybe HelpPage -> Widget Name
helpPageWidget path mhp =
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
  labels = plainBorder & topLabels .~ (plainHBorder & centerLabel ?~ txt (toText title))

  title :: Document (Syntax Raw)
  title = case mhp of
    Nothing -> "Page not found"
    Just hp -> fromMaybe "Untitled" (hp ^. helpMetadata . at "title")

  content :: Widget Name
  content = case mhp of
    Nothing -> padTop (Pad 2) . hCenter . txt $ "No help page exists at path " <> T.pack path
    Just hp -> drawMarkdown (hp ^. helpDoc)
