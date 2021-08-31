{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.TUI.Attr where

import           Brick
import           Brick.Forms
import qualified Graphics.Vty as V

robotAttr, plantAttr, flowerAttr, dirtAttr, rockAttr, baseAttr, highlightAttr, defAttr :: AttrName
robotAttr     = "robotAttr"
entityAttr    = "entityAttr"
plantAttr     = "plantAttr"
flowerAttr    = "flowerAttr"
dirtAttr      = "dirtAttr"
rockAttr      = "rockAttr"
baseAttr      = "baseAttr"
highlightAttr = "highlightAttr"
defAttr       = "defAttr"

theAttrMap :: AttrMap
theAttrMap = attrMap V.defAttr
  [ (robotAttr, fg V.white `V.withStyle` V.bold)
  , (entityAttr, fg V.white)
  , (plantAttr, fg V.green)
  , (flowerAttr, fg V.yellow)
  , (dirtAttr, fg (V.rgbColor @Int 165 42 42))
  , (rockAttr, fg (V.rgbColor @Int 80 80 80))
  , (highlightAttr, fg V.cyan)
  , (invalidFormInputAttr, fg V.red)
  , (focusedFormInputAttr, V.defAttr)
  , (defAttr, V.defAttr)
  ]
