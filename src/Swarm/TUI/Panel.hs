-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.TUI.Panel
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A small custom "panel widget" for use in the Swarm TUI. Panels draw
-- a border around some content, with the color of the border
-- depending on whether the panel is currently focused.  Panels exist
-- within a 'FocusRing' such that the user can cycle between the
-- panels (using /e.g./ the @Tab@ key).
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Swarm.TUI.Panel
  ( panel
  ) where

import           Brick
import           Brick.Focus
import           Brick.Widgets.Border

import           Control.Lens

data Panel n = Panel { _panelName :: n, _panelContent :: Widget n }

makeLenses ''Panel

instance Named (Panel n) n where
  getName = view panelName

drawPanel :: Eq n => AttrName -> FocusRing n -> Panel n -> Widget n
drawPanel attr fr = withFocusRing fr drawPanel'
  where
    drawPanel' :: Bool -> Panel n -> Widget n
    drawPanel' focused p
      = (if focused then overrideAttr borderAttr attr else id)
      $ border (p ^. panelContent)

-- | Create a panel.
panel :: Eq n => AttrName -- ^ Border attribute to use when the panel is focused.
  -> FocusRing n          -- ^ Focus ring the panel should be part of.
  -> n                    -- ^ The name of the panel. Must be unique.
  -> Widget n             -- ^ The content of the panel.
  -> Widget n
panel attr fr nm w = drawPanel attr fr (Panel nm w)
