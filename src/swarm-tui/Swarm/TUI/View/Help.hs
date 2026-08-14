{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the help system.
module Swarm.TUI.View.Help (drawHelpUI) where

import Brick
import Control.Lens
import Data.Maybe (fromMaybe)
import Swarm.Language.Help (HelpPage, helpDoc, helpMetadata)
import Swarm.Language.Syntax (Phase (Raw), Syntax)
import Swarm.ResourceLoading (Collection, atPath)
import Swarm.TUI.Border (BorderLabels, borderWithLabels, centerLabel, plainBorder, plainHBorder, topLabels)
import Swarm.TUI.Model (Name (..))
import Swarm.TUI.View.Util (drawMarkdown)
import Swarm.Text.Markdown (Document, toText)

drawHelpUI :: Collection HelpPage -> FilePath -> [Widget Name]
drawHelpUI help hp = [helpPageWidget (help ^? atPath hp)]

-- XXX move this somewhere else, i.e. Swarm.TUI.View.Help
helpPageWidget :: Maybe HelpPage -> Widget Name
helpPageWidget = \case
  Nothing -> txt "Error, help page not found" -- XXX improve me
  Just p ->
    borderWithLabels labels
      . withVScrollBars OnRight
      . viewport HelpViewport Vertical
      -- Pad 1 on the right first, before padRight Max, to make sure
      -- there is always at least one space of padding
      . padRight Max
      . padRight (Pad 1)
      . padLeft (Pad 1)
      . padTop (Pad 1)
      $ drawMarkdown (p ^. helpDoc)
   where
    labels :: BorderLabels Name
    labels = plainBorder & topLabels .~ (plainHBorder & centerLabel ?~ txt (toText title))

    title :: Document (Syntax Raw)
    title = fromMaybe mempty (p ^. helpMetadata . at "title")
