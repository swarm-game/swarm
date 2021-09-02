-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.TUI.Attr
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering attributes (/i.e./ foreground and background colors,
-- styles, /etc./) used by the Swarm TUI.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.TUI.Attr where

import           Brick
import           Brick.Forms
import qualified Graphics.Vty as V

-- | A mapping from the defined attribute names to TUI attributes.
swarmAttrMap :: AttrMap
swarmAttrMap = attrMap V.defAttr

  -- World rendering attributes
  [ (robotAttr,     fg V.white `V.withStyle` V.bold)
  , (entityAttr,    fg V.white)
  , (plantAttr,     fg V.green)
  , (dirtAttr,      fg (V.rgbColor @Int 165 42 42))
  , (rockAttr,      fg (V.rgbColor @Int 80 80 80))
  , (deviceAttr,    fg V.yellow `V.withStyle` V.bold)

  -- UI rendering attributes
  , (highlightAttr, fg V.cyan)
  , (invalidFormInputAttr, fg V.red)
  , (focusedFormInputAttr, V.defAttr)

  -- Default attribute
  , (defAttr, V.defAttr)
  ]

-- | Some defined attribute names used in the Swarm TUI.
robotAttr, entityAttr, plantAttr, flowerAttr, dirtAttr, rockAttr, baseAttr,
  deviceAttr,
  highlightAttr, defAttr :: AttrName
robotAttr     = "robotAttr"
entityAttr    = "entityAttr"
plantAttr     = "plantAttr"
flowerAttr    = "flowerAttr"
dirtAttr      = "dirtAttr"
rockAttr      = "rockAttr"
baseAttr      = "baseAttr"
deviceAttr    = "deviceAttr"
highlightAttr = "highlightAttr"
defAttr       = "defAttr"
