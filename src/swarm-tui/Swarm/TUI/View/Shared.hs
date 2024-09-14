{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- UI view components shared across dialogs
module Swarm.TUI.View.Shared where

import Brick
import Brick.Widgets.Center (hCenter)
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Attribute.Attr (italicAttr)

tabControlFooter :: Widget Name
tabControlFooter = hCenter $ withAttr italicAttr $ txt "NOTE: [Tab] toggles focus between panes"
