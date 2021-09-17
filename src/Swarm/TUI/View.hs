-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.TUI.View
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the TUI.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.View where

import           Control.Arrow         ((&&&))
import           Control.Lens
import           Data.Array            (range)
import           Data.List.Split       (chunksOf)
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Linear
import           Text.Printf

import           Brick                 hiding (Direction)
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Border  (hBorder, hBorderWithLabel)
import           Brick.Widgets.Center  (center, hCenter)
import           Brick.Widgets.Dialog
import qualified Brick.Widgets.List    as BL

import           Swarm.Game.Display
import           Swarm.Game.Entity     hiding (empty)
import           Swarm.Game.Recipe
import           Swarm.Game.Robot
import           Swarm.Game.State
import           Swarm.Game.Terrain    (displayTerrain)
import qualified Swarm.Game.World      as W
import           Swarm.Language.Syntax
import           Swarm.TUI.Attr
import           Swarm.TUI.Model
import           Swarm.TUI.Panel
import           Swarm.Util

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ drawDialog (s ^. uiState)
  , joinBorders $
    hBox
    [ hLimitPercent 25 $ panel highlightAttr fr InfoPanel Nothing $
      drawInfoPanel s
    , vBox
      [ panel highlightAttr fr WorldPanel (Just (padLeftRight 1 $ drawTPS s)) $
        drawWorld (s ^. gameState)
      , drawMenu
          (s ^. gameState . paused)
          ((s ^. gameState . viewCenterRule) == VCRobot "base")
          (s ^. uiState)
      , panel highlightAttr fr REPLPanel Nothing $
        vLimit replHeight $
        padBottom Max $ padLeftRight 1 $
        drawRepl s
      ]
    ]
  ]
  where
    fr = s ^. uiState . uiFocusRing

drawTPS :: AppState -> Widget Name
drawTPS s = hBox [tpsInfo, txt " ", tpfInfo]
  where
    tpsInfo
      | l >= 0    = hBox [str (show n), txt " ", txt (number n "tick"), txt " / s"]
      | otherwise = hBox [txt "1 tick / ", str (show n), txt " s"]

    tpfInfo = hBox [txt "(", str (show (s ^. uiState . ticksPerFrame)), txt " ticks/frame)"]

    l = s ^. uiState . lgTicksPerSecond
    n = 2^abs l

replHeight :: Int
replHeight = 10

errorDialog :: Dialog ()
errorDialog = dialog (Just "Error") Nothing 80

drawDialog :: UIState -> Widget Name
drawDialog s = case s ^. uiError of
  Nothing -> emptyWidget
  Just d  -> renderDialog errorDialog d

drawMenu :: Bool -> Bool -> UIState -> Widget Name
drawMenu isPaused viewingBase
  = vLimit 1
  . hBox . map (padLeftRight 1 . drawKeyCmd)
  . (globalKeyCmds++) . keyCmdsFor . focusGetCurrent . view uiFocusRing
  where
    globalKeyCmds =
      [ ("^q", "quit")
      , ("Tab", "cycle panels")
      ]
    keyCmdsFor (Just REPLPanel) =
      [ ("Enter", "execute")
      ]
    keyCmdsFor (Just WorldPanel) =
      [ ("←↓↑→ / hjkl", "scroll")
      , ("<>", "slower/faster")
      , ("p", if isPaused then "unpause" else "pause")
      ]
      ++
      [ ("s", "step") | isPaused ]
      ++
      [ ("c", "recenter") | not viewingBase ]

    keyCmdsFor (Just InfoPanel)  =
      [ ("↓↑/jk/Pg{Up,Dn}/Home/End", "navigate")
      , ("Enter", "make")
      ]
    keyCmdsFor _ = []

drawKeyCmd :: (Text, Text) -> Widget Name
drawKeyCmd (key, cmd) = txt $ T.concat [ "[", key, "] ", cmd ]

drawWorld :: GameState -> Widget Name
drawWorld g
  = center
  $ cached WorldCache
  $ reportExtent WorldExtent
  $ Widget Fixed Fixed $ do
    ctx <- getContext
    let w   = ctx ^. availWidthL
        h   = ctx ^. availHeightL
        ixs = range (viewingRegion g (w,h))
    render . vBox . map hBox . chunksOf w . map drawLoc $ ixs
  where
    -- XXX update how this works!  Gather all displays, all
    -- entities...  Should make a Display remember which is the
    -- currently selected char (based on orientation); Entity lens for
    -- setting orientation updates the Display too.  Then we can just
    -- get all the Displays for each cell, make a monoid based on
    -- priority.

    robotsByLoc
      = M.fromListWith (maxOn (^. robotDisplay . displayPriority)) . map (view robotLocation &&& id)
      . M.elems $ g ^. robotMap
    drawLoc (row,col) = case M.lookup (V2 col (-row)) robotsByLoc of
      Just r  -> withAttr (r ^. robotDisplay . displayAttr) $
        str [lookupDisplay ((r ^. robotOrientation) >>= toDirection) (r ^. robotDisplay)]
      Nothing -> drawCell (row,col) (g ^. world)

drawCell :: W.Worldly w Int Entity => (Int, Int) -> w -> Widget Name
drawCell i w = case W.lookupEntity i w of
  Just e  -> displayEntity e
  Nothing -> displayTerrain (toEnum (W.lookupTerrain i w))

drawInfoPanel :: AppState -> Widget Name
drawInfoPanel s
  = vBox
    [ drawRobotInfo s
    , hBorder
    , vLimitPercent 50 $ padBottom Max $ padAll 1 $ drawMessageBox s
    ]

drawMessageBox :: AppState -> Widget Name
drawMessageBox s = case s ^. uiState . uiFocusRing . to focusGetCurrent of
  Just InfoPanel -> explainFocusedItem s
  _              -> drawMessages (s ^. gameState . messageQueue)

explainFocusedItem :: AppState -> Widget Name
explainFocusedItem s = case mItem of
  Nothing                   -> txt " "
  Just (Separator _)        -> txt " "
  Just (InventoryEntry _ e) -> vBox $
    map (padBottom (Pad 1) . txtWrap) (e ^. entityDescription)
    ++
    explainRecipes e
  where
    mList = s ^? uiState . uiInventory . _Just . _2
    mItem = mList >>= BL.listSelectedElement >>= (Just . snd)

    explainRecipes :: Entity -> [Widget Name]
    explainRecipes = map (txt . prettyRecipe) . recipesWith

    recipesWith :: Entity -> [Recipe Entity]
    recipesWith e = S.toList . S.fromList $
         recipesFor (s ^. gameState . recipesOut) e
      ++ recipesFor (s ^. gameState . recipesIn) e
      -- We remove duplicates by converting to and from a Set,
      -- because some recipes can have an item as both an input and an
      -- output (e.g. some recipes that require a furnace); those
      -- recipes would show up twice above.

drawMessages :: [Text] -> Widget Name
drawMessages [] = txt " "
drawMessages ms = Widget Fixed Fixed $ do
  ctx <- getContext
  let h   = ctx ^. availHeightL
  render . vBox . map txt . reverse . take h $ ms

drawRobotInfo :: AppState -> Widget Name
drawRobotInfo s = case (s ^. gameState . to focusedRobot, s ^. uiState . uiInventory) of
  (Just r, Just (_, lst)) ->
    let V2 x y = r ^. robotLocation in
    padBottom Max
    $ vBox
    [ hCenter $ hBox
        [ txt (r ^. robotName)
        , padLeft (Pad 2) $ str (printf "(%d, %d)" x y)
        , padLeft (Pad 2) $ displayEntity (r ^. robotEntity)
        ]
    , padAll 1 (BL.renderListWithIndex (drawItem (lst ^. BL.listSelectedL)) isFocused lst)
    ]
  _ -> padBottom Max $ str " "
  where
    isFocused = (s ^. uiState . uiFocusRing . to focusGetCurrent) == Just InfoPanel

drawItem :: Maybe Int -> Int -> Bool -> InventoryEntry -> Widget Name
drawItem sel i _ (Separator l)
  -- Make sure a separator right before the focused element is visible
  = forceAttr sepAttr ((if sel == Just (i+1) then visible else id) $ hBorderWithLabel (txt l))
drawItem _ _ _ (InventoryEntry n e) = drawLabelledEntityName e <+> showCount n
  where
    showCount = padLeft Max . str . show

drawLabelledEntityName :: Entity -> Widget Name
drawLabelledEntityName e = hBox
  [ padRight (Pad 2) (displayEntity e)
  , txt (e ^. entityName)
  ]

drawRepl :: AppState -> Widget Name
drawRepl s = vBox $
  map fmt (reverse (take (replHeight - 1) . filter newEntry $ (s ^. uiState . uiReplHistory)))
  ++
  case isActive <$> (s ^. gameState . robotMap . at "base") of
    Just False -> [ renderForm (s ^. uiState . uiReplForm) ]
    _          -> [ padRight Max $ txt "..." ]
  where
    newEntry (REPLEntry False _) = False
    newEntry _                   = True

    fmt (REPLEntry _ e) = txt replPrompt <+> txt e
    fmt (REPLOutput t)  = txt t

