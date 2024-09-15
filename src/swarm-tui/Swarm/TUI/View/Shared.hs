{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- UI view components shared across dialogs
module Swarm.TUI.View.Shared where

import Brick
import Brick.Widgets.Center (hCenter)
import Swarm.TUI.View.Attribute.Attr (italicAttr)

tabControlFooter :: Widget n
tabControlFooter = hCenter $ withAttr italicAttr $ txt "NOTE: [Tab] toggles focus between panes"
