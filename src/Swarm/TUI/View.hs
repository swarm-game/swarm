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

module Swarm.TUI.View
  (
    drawUI
  , drawTPS

    -- * Error dialog
  , errorDialog
  , drawDialog

    -- * Key hint menu
  , drawMenu
  , drawKeyCmd

    -- * World
  , drawWorld
  , drawCell

    -- * Info panel

  , drawInfoPanel
  , drawMessageBox
  , explainFocusedItem
  , drawMessages
  , drawRobotInfo
  , drawItem
  , drawLabelledEntityName

    -- * REPL
  , drawREPL
  ) where

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

-- | The main entry point for drawing the entire UI.  Generates a list
--   of widgets, where each represents a layer.  Right now we just
--   generate two layers: the main layer and a floating dialog that
--   can be on top.
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
        drawREPL s
      ]
    ]
  ]
  where
    fr = s ^. uiState . uiFocusRing

-- | Draw info about the current number of ticks per second.
drawTPS :: AppState -> Widget Name
drawTPS s = hBox [tpsInfo, txt " ", tpfInfo]
  where
    tpsInfo
      | l >= 0    = hBox [str (show n), txt " ", txt (number n "tick"), txt " / s"]
      | otherwise = hBox [txt "1 tick / ", str (show n), txt " s"]

    tpfInfo = hBox [txt "(", str (printf "%0.1f" (s ^. uiState . ticksPerFrame)), txt " ticks/frame)"]

    l = s ^. uiState . lgTicksPerSecond
    n = 2^abs l

-- | The height of the REPL box.  Perhaps in the future this should be
--   configurable.
replHeight :: Int
replHeight = 10

-- | The error dialog window.
errorDialog :: Dialog ()
errorDialog = dialog (Just "Error") Nothing 80

-- | Draw the error dialog window, if it should be displayed right now.
drawDialog :: UIState -> Widget Name
drawDialog s = case s ^. uiError of
  Nothing -> emptyWidget
  Just d  -> renderDialog errorDialog d

-- | Draw a menu explaining what key commands are available for the
--   current panel.  This menu is displayed as a single line in
--   between the world panel and the REPL.
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
      [ ("↓↑/Pg{Up,Dn}/Home/End", "navigate")
      , ("Enter", "make")
      ]
    keyCmdsFor _ = []

-- | Draw a single key command in the menu.
drawKeyCmd :: (Text, Text) -> Widget Name
drawKeyCmd (key, cmd) = txt $ T.concat [ "[", key, "] ", cmd ]

-- | Draw the current world view.
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

-- | Draw a single cell of the world.
drawCell :: (Int, Int) -> W.World Int Entity -> Widget Name
drawCell i w = case W.lookupEntity i w of
  Just e  -> displayEntity e
  Nothing -> displayTerrain (toEnum (W.lookupTerrain i w))

-- | Draw the info panel on the left-hand side of the UI.
drawInfoPanel :: AppState -> Widget Name
drawInfoPanel s
  = vBox
    [ drawRobotInfo s
    , hBorder
    , vLimitPercent 50 $ padBottom Max $ padAll 1 $ drawMessageBox s
    ]

-- | Draw the lower half of the info panel, which either shows info
--   about the currently focused inventory item, or shows the most
--   recent entries in the message queue.
drawMessageBox :: AppState -> Widget Name
drawMessageBox s = case s ^. uiState . uiFocusRing . to focusGetCurrent of
  Just InfoPanel -> explainFocusedItem s
  _              -> drawMessages (s ^. gameState . messageQueue)

-- | Display info about the currently focused inventory entity,
--   such as its description and relevant recipes.
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

-- | Draw a list of messages.
drawMessages :: [Text] -> Widget Name
drawMessages [] = txt " "
drawMessages ms = Widget Fixed Fixed $ do
  ctx <- getContext
  let h   = ctx ^. availHeightL
  render . vBox . map txt . reverse . take h $ ms

-- | Draw info about the currently focused robot, such as its name,
--   position, orientation, and inventory.
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

-- | Draw an inventory entry.
drawItem :: Maybe Int -- ^ The index of the currently selected inventory entry
  -> Int              -- ^ The index of the entry we are drawing
  -> Bool             -- ^ Whether this entry is selected; we can ignore this
                      --   because it will automatically have a special attribute
                      --   applied to it.
  -> InventoryEntry   -- ^ The entry to draw.
  -> Widget Name
drawItem sel i _ (Separator l)

  -- Make sure a separator right before the focused element is
  -- visible. Otherwise, when a separator occurs as the very first
  -- element of the list, once it scrolls off the top of the viewport
  -- it will never become visible again.
  -- See https://github.com/jtdaugherty/brick/issues/336#issuecomment-921220025
  = forceAttr sepAttr ((if sel == Just (i+1) then visible else id) $ hBorderWithLabel (txt l))

drawItem _ _ _ (InventoryEntry n e) = drawLabelledEntityName e <+> showCount n
  where
    showCount = padLeft Max . str . show

-- | Draw the name of an entity, labelled with its visual
--   representation as a cell in the world.
drawLabelledEntityName :: Entity -> Widget Name
drawLabelledEntityName e = hBox
  [ padRight (Pad 2) (displayEntity e)
  , txt (e ^. entityName)
  ]

-- | Draw the REPL.
drawREPL :: AppState -> Widget Name
drawREPL s = vBox $
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

