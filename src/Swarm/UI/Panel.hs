{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Swarm.UI.Panel where

import           Brick
import           Brick.Focus
import           Brick.Widgets.Border

import           Control.Lens

data Panel n = Panel { _panelName :: n, _panelContent :: Widget n }

makeLenses ''Panel

instance Named (Panel n) n where
  getName = view panelName

drawPanel :: Eq n => AttrName -> FocusRing n -> Panel n -> Widget n
drawPanel attr fr p = withFocusRing fr drawPanel' p
  where
    drawPanel' :: Bool -> Panel n -> Widget n
    drawPanel' focused p
      = (if focused then overrideAttr borderAttr attr else id)
      $ border (p ^. panelContent)

panel :: Eq n => AttrName -> FocusRing n -> n -> Widget n -> Widget n
panel attr fr nm w = drawPanel attr fr (Panel nm w)
