{-# LANGUAGE OverloadedStrings #-}

module Swarm.UI.Attr where

import           Brick
import           Brick.Forms
import qualified Graphics.Vty as V

robotAttr, plantAttr, flowerAttr, dirtAttr, rockAttr, baseAttr, highlightAttr, defAttr :: AttrName
robotAttr     = "robotAttr"
plantAttr     = "plantAttr"
flowerAttr    = "flowerAttr"
dirtAttr      = "dirtAttr"
rockAttr      = "rockAttr"
baseAttr      = "baseAttr"
highlightAttr = "highlightAttr"
defAttr       = "defAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (robotAttr, fg V.white `V.withStyle` V.bold)
  , (plantAttr, fg V.green)
  , (flowerAttr, fg V.yellow)
  , (dirtAttr, fg (V.rgbColor 165 42 42))
  , (rockAttr, fg (V.rgbColor 80 80 80))
  , (highlightAttr, fg V.cyan)
  , (invalidFormInputAttr, fg V.red)
  , (focusedFormInputAttr, V.defAttr)
  , (defAttr, V.defAttr)
  ]
