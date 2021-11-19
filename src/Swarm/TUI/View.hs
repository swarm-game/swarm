{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.TUI.View
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the TUI.
module Swarm.TUI.View (
  drawUI,
  drawTPS,

  -- * Error dialog
  errorDialog,
  drawDialog,
  chooseCursor,

  -- * Key hint menu
  drawMenu,
  drawKeyCmd,

  -- * World
  drawWorld,
  drawCell,

  -- * Robot panel
  drawRobotPanel,
  drawItem,
  drawLabelledEntityName,

  -- * Info panel
  drawInfoPanel,
  explainFocusedItem,

  -- * REPL
  drawREPL,
) where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Array (range)
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Linear
import Text.Printf
import Text.Wrap

import Brick hiding (Direction)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Dialog
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Table as BT

import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Terrain (displayTerrain)
import qualified Swarm.Game.World as W
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Model
import Swarm.TUI.Panel
import Swarm.Util

-- | The main entry point for drawing the entire UI.  Generates a list
--   of widgets, where each represents a layer.  Right now we just
--   generate two layers: the main layer and a floating dialog that
--   can be on top.
drawUI :: AppState -> [Widget Name]
drawUI s =
  [ drawDialog (s ^. uiState)
  , joinBorders $
      hBox
        [ hLimitPercent 25 $
            vBox
              [ vLimitPercent 50 $ panel highlightAttr fr RobotPanel plainBorder $ drawRobotPanel s
              , panel
                  highlightAttr
                  fr
                  InfoPanel
                  ( plainBorder
                      & topLabels . centerLabel
                      .~ (if moreTop then Just (txt " · · · ") else Nothing)
                      & bottomLabels . centerLabel
                      .~ (if moreBot then Just (txt " · · · ") else Nothing)
                  )
                  $ drawInfoPanel s
              ]
        , vBox
            [ panel
                highlightAttr
                fr
                WorldPanel
                (plainBorder & bottomLabels . rightLabel ?~ padLeftRight 1 (drawTPS s))
                (drawWorld $ s ^. gameState)
            , drawMenu s
            , panel
                highlightAttr
                fr
                REPLPanel
                ( plainBorder
                    & topLabels . rightLabel .~ (drawType <$> (s ^. uiState . uiReplType))
                )
                ( vLimit replHeight $
                    padBottom Max $
                      padLeftRight 1 $
                        drawREPL s
                )
            ]
        ]
  ]
 where
  fr = s ^. uiState . uiFocusRing
  moreTop = s ^. uiState . uiMoreInfoTop
  moreBot = s ^. uiState . uiMoreInfoBot

-- | Render the type of the current REPL input to be shown to the user.
drawType :: Polytype -> Widget Name
drawType = withAttr infoAttr . padLeftRight 1 . txt . prettyText

-- | Draw info about the current number of ticks per second.
drawTPS :: AppState -> Widget Name
drawTPS s = hBox (tpsInfo : rateInfo)
 where
  tpsInfo
    | l >= 0 = hBox [str (show n), txt " ", txt (number n "tick"), txt " / s"]
    | otherwise = hBox [txt "1 tick / ", str (show n), txt " s"]

  rateInfo
    | s ^. uiState . uiShowFPS =
      [ txt " ("
      , str (printf "%0.1f" (s ^. uiState . uiTPF))
      , txt " tpf, "
      , str (printf "%0.1f" (s ^. uiState . uiFPS))
      , txt " fps)"
      ]
    | otherwise = []

  l = s ^. uiState . lgTicksPerSecond
  n = 2 ^ abs l

-- | The height of the REPL box.  Perhaps in the future this should be
--   configurable.
replHeight :: Int
replHeight = 10

-- | Hide the cursor when a modal is set
chooseCursor :: AppState -> [CursorLocation n] -> Maybe (CursorLocation n)
chooseCursor s locs = case s ^. uiState . uiModal of
  Nothing -> showFirstCursor s locs
  Just _ -> Nothing

-- | The error dialog window.
errorDialog :: Dialog ()
errorDialog = dialog (Just "Error") Nothing 80

-- | Render a fullscreen widget with some padding
renderModal :: Modal -> Widget Name
renderModal modal = renderDialog (dialog (Just modalTitle) Nothing 500) modalWidget
 where
  modalWidget = Widget Fixed Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
        h = ctx ^. availHeightL
        padding = 10
    render $ setAvailableSize (w - padding, h - padding) modalContent
  (modalTitle, modalContent) =
    case modal of
      HelpModal -> ("Help", helpWidget)
      WinModal -> ("", txt "Congratulations!")

helpWidget :: Widget Name
helpWidget = (helpKeys <=> fill ' ') <+> (helpCommands <=> fill ' ')
 where
  helpKeys =
    vBox
      [ hCenter $ txt "Global Keybindings"
      , hCenter $ mkTable glKeyBindings
      ]
  mkTable = BT.renderTable . BT.table . map toWidgets
  toWidgets (k, v) = [txt k, txt v]
  glKeyBindings =
    [ ("F1", "Help")
    , ("Ctrl-q", "quit the game")
    , ("Tab", "cycle panel focus")
    , ("Meta-w", "focus on the world map")
    , ("Meta-e", "focus on the robot inventory")
    , ("Meta-r", "focus on the REPL")
    , ("Meta-t", "focus on the info panel")
    ]
  helpCommands =
    vBox
      [ hCenter $ txt "Commands"
      , hCenter $ mkTable baseCommands
      ]
  baseCommands =
    [ ("build <name> {<commands>}", "Create a robot")
    , ("make <name>", "Craft an item")
    , ("move", "Move one step in the current direction")
    , ("turn <dir>", "Change the current direction")
    , ("grab", "Grab whatver is available")
    , ("give <robot> <item>", "Give an item to another robot")
    , ("has <item>", "Check for an item in the inventory")
    ]

-- | Draw the error dialog window, if it should be displayed right now.
drawDialog :: UIState -> Widget Name
drawDialog s = case s ^. uiModal of
  Just m -> renderModal m
  Nothing -> case s ^. uiError of
    Just d -> renderDialog errorDialog d
    Nothing -> emptyWidget

-- | Draw a menu explaining what key commands are available for the
--   current panel.  This menu is displayed as a single line in
--   between the world panel and the REPL.
drawMenu :: AppState -> Widget Name
drawMenu s =
  vLimit 1
    . hBox
    . (++ [gameModeWidget])
    . map (padLeftRight 1 . drawKeyCmd)
    . (globalKeyCmds ++)
    . keyCmdsFor
    . focusGetCurrent
    . view (uiState . uiFocusRing)
    $ s
 where
  isReplWorking = s ^. gameState . replWorking
  isPaused = s ^. gameState . paused
  viewingBase = (s ^. gameState . viewCenterRule) == VCRobot "base"
  mode = s ^. gameState . gameMode

  gameModeWidget =
    padLeft Max . padLeftRight 1
      . txt
      . (<> " mode")
      $ case mode of
        ClassicMode -> "Classic"
        CreativeMode -> "Creative"
        ChallengeMode -> "Challenge"
  globalKeyCmds =
    [ ("F1", "help")
    , ("Tab", "cycle panels")
    ]
  keyCmdsFor (Just REPLPanel) =
    [ ("↓↑", "history")
    ]
      ++ [("Enter", "execute") | not isReplWorking]
      ++ [("^c", "cancel") | isReplWorking]
  keyCmdsFor (Just WorldPanel) =
    [ ("←↓↑→ / hjkl", "scroll") | mode == CreativeMode
    ]
      ++ [ ("<>", "slower/faster")
         , ("p", if isPaused then "unpause" else "pause")
         ]
      ++ [("s", "step") | isPaused]
      ++ [("c", "recenter") | not viewingBase]
  keyCmdsFor (Just RobotPanel) =
    [ ("↓↑/Pg{Up,Dn}/Home/End/jk", "navigate")
    , ("Enter", "make")
    ]
  keyCmdsFor (Just InfoPanel) =
    [ ("↓↑/Pg{Up,Dn}/Home/End/jk", "scroll")
    ]
  keyCmdsFor _ = []

-- | Draw a single key command in the menu.
drawKeyCmd :: (Text, Text) -> Widget Name
drawKeyCmd (key, cmd) = txt $ T.concat ["[", key, "] ", cmd]

------------------------------------------------------------
-- World panel
------------------------------------------------------------

-- | Draw the current world view.
drawWorld :: GameState -> Widget Name
drawWorld g =
  center $
    cached WorldCache $
      reportExtent WorldExtent $
        Widget Fixed Fixed $ do
          ctx <- getContext
          let w = ctx ^. availWidthL
              h = ctx ^. availHeightL
              ixs = range (viewingRegion g (fromIntegral w, fromIntegral h))
          render . vBox . map hBox . chunksOf w . map drawLoc $ ixs
 where
  -- XXX update how this works!  Gather all displays, all
  -- entities...  Should make a Display remember which is the
  -- currently selected char (based on orientation); Entity lens for
  -- setting orientation updates the Display too.  Then we can just
  -- get all the Displays for each cell, make a monoid based on
  -- priority.

  robotsByLoc =
    M.fromListWith (maxOn (^. robotDisplay . displayPriority)) . map (view robotLocation &&& id)
      . M.elems
      $ g ^. robotMap

  drawLoc :: W.Coords -> Widget Name
  drawLoc coords =
    let (ePrio, eWidget) = drawCell hiding (g ^. world) coords
        hiding =
          if g ^. gameMode == CreativeMode
            then HideNoEntity
            else maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g
     in case M.lookup (W.coordsToLoc coords) robotsByLoc of
          Just r
            | ePrio > (r ^. robotDisplay . displayPriority) -> eWidget
            | otherwise ->
              withAttr (r ^. robotDisplay . displayAttr) $
                str [lookupDisplay ((r ^. robotOrientation) >>= toDirection) (r ^. robotDisplay)]
          Nothing -> eWidget

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

-- | Draw a single cell of the world, either hiding entities that current robot does not know,
--   or hiding all/none depending on Left value (True/False).
drawCell :: HideEntity -> W.World Int Entity -> W.Coords -> (Int, Widget Name)
drawCell edr w i = case W.lookupEntity i w of
  Nothing -> (0, displayTerrain (toEnum (W.lookupTerrain i w)))
  Just e ->
    ( e ^. entityDisplay . displayPriority
    , displayEntity (hide e)
    )
 where
  known e =
    e `hasProperty` Known
      || case edr of
        HideAllEntities -> False
        HideNoEntity -> True
        HideEntityUnknownTo ro -> ro `robotKnows` e
  hide e = (if known e then id else entityDisplay . defaultChar %~ const '?') e

------------------------------------------------------------
-- Robot inventory panel
------------------------------------------------------------

-- | Draw info about the currently focused robot, such as its name,
--   position, orientation, and inventory.
drawRobotPanel :: AppState -> Widget Name
drawRobotPanel s = case (s ^. gameState . to focusedRobot, s ^. uiState . uiInventory) of
  (Just r, Just (_, lst)) ->
    let V2 x y = r ^. robotLocation
     in padBottom Max $
          vBox
            [ hCenter $
                hBox
                  [ txt (r ^. robotName)
                  , padLeft (Pad 2) $ str (printf "(%d, %d)" x y)
                  , padLeft (Pad 2) $ displayEntity (r ^. robotEntity)
                  ]
            , padAll 1 (BL.renderListWithIndex (drawItem (lst ^. BL.listSelectedL)) True lst)
            ]
  _ -> padRight Max . padBottom Max $ str " "

-- | Draw an inventory entry.
drawItem ::
  -- | The index of the currently selected inventory entry
  Maybe Int ->
  -- | The index of the entry we are drawing
  Int ->
  -- | Whether this entry is selected; we can ignore this
  --   because it will automatically have a special attribute
  --   applied to it.
  Bool ->
  -- | The entry to draw.
  InventoryListEntry ->
  Widget Name
drawItem sel i _ (Separator l) =
  -- Make sure a separator right before the focused element is
  -- visible. Otherwise, when a separator occurs as the very first
  -- element of the list, once it scrolls off the top of the viewport
  -- it will never become visible again.
  -- See https://github.com/jtdaugherty/brick/issues/336#issuecomment-921220025
  (if sel == Just (i + 1) then visible else id) $ hBorderWithLabel (txt l)
drawItem _ _ _ (InventoryEntry n e) = drawLabelledEntityName e <+> showCount n
 where
  showCount = padLeft Max . str . show
drawItem _ _ _ (InstalledEntry e) = drawLabelledEntityName e <+> padLeft Max (str " ")

-- | Draw the name of an entity, labelled with its visual
--   representation as a cell in the world.
drawLabelledEntityName :: Entity -> Widget Name
drawLabelledEntityName e =
  hBox
    [ padRight (Pad 2) (displayEntity e)
    , txt (e ^. entityName)
    ]

------------------------------------------------------------
-- Info panel
------------------------------------------------------------

-- | Draw the info panel in the bottom-left corner, which shows info
--   about the currently focused inventory item.
drawInfoPanel :: AppState -> Widget Name
drawInfoPanel s =
  viewport InfoViewport Vertical
    . padLeftRight 1
    $ explainFocusedItem s

-- | Display info about the currently focused inventory entity,
--   such as its description and relevant recipes.
explainFocusedItem :: AppState -> Widget Name
explainFocusedItem s = case mItem of
  Nothing -> txt " "
  Just (Separator _) -> txt " "
  Just (InventoryEntry _ e) ->
    vBox (map (padBottom (Pad 1) . txtWrap) (e ^. entityDescription))
      <=> explainRecipes e
  Just (InstalledEntry e) ->
    vBox (map (padBottom (Pad 1) . txtWrap) (e ^. entityDescription))
      <=> explainRecipes e
      -- Special case: installed logger device displays the robot's log.
      <=> if e ^. entityName == "logger" then drawRobotLog s else emptyWidget
 where
  mList = s ^? uiState . uiInventory . _Just . _2
  mItem = mList >>= BL.listSelectedElement >>= (Just . snd)

  explainRecipes :: Entity -> Widget Name
  explainRecipes e
    | null recipes = emptyWidget
    | otherwise =
      vBox
        [ padBottom (Pad 1) (hBorderWithLabel (txt "Recipes"))
        , padLeftRight 2 $
            hCenter $
              vBox $
                map (hLimit widthLimit . padBottom (Pad 1) . drawRecipe e inv) recipes
        ]
   where
    recipes = recipesWith e

    inv = fromMaybe E.empty $ s ^? gameState . to focusedRobot . _Just . robotInventory

    width (n, ingr) =
      length (show n) + 1 + maximum0 (map T.length . T.words $ ingr ^. entityName)

    maxInputWidth =
      fromMaybe 0 $
        maximumOf (traverse . recipeInputs . traverse . to width) recipes
    maxOutputWidth =
      fromMaybe 0 $
        maximumOf (traverse . recipeOutputs . traverse . to width) recipes
    widthLimit = 2 * max maxInputWidth maxOutputWidth + 11

  recipesWith :: Entity -> [Recipe Entity]
  recipesWith e =
    let getRecipes select = recipesFor (s ^. gameState . select) e
     in L.nub $ getRecipes recipesOut ++ getRecipes recipesIn

-- | Draw an ASCII art representation of a recipe.
drawRecipe :: Entity -> Inventory -> Recipe Entity -> Widget Name
drawRecipe e inv (Recipe ins outs reqs time) =
  vBox
    -- any requirements (e.g. furnace) go on top.
    [ hCenter $ drawReqs reqs
    , -- then we draw inputs, a connector, and outputs.
      hBox
        [ vBox (zipWith drawIn [0 ..] (ins <> times))
        , connector
        , vBox (zipWith drawOut [0 ..] outs)
        ]
    ]
 where
  -- The connector is either just a horizontal line ─────
  -- or, if there are requirements, a horizontal line with
  -- a vertical piece coming out of the center, ──┴── .
  connector
    | null reqs = hLimit 5 hBorder
    | otherwise =
      hBox
        [ hLimit 2 hBorder
        , joinableBorder (Edges True False True True)
        , hLimit 2 hBorder
        ]
  inLen = length ins + length times
  outLen = length outs
  times = [(fromIntegral time, timeE) | time /= 1]

  -- Draw inputs and outputs.
  drawIn, drawOut :: Int -> (Count, Entity) -> Widget Name
  drawIn i (n, ingr) =
    hBox
      [ padRight (Pad 1) $ str (show n) -- how many?
      , fmtEntityName missing ingr -- name of the input
      , padLeft (Pad 1) $ -- a connecting line:   ─────┬
          hBorder
            <+> ( joinableBorder (Edges (i /= 0) (i /= inLen -1) True False) -- ...maybe plus vert ext:   │
                    <=> if i /= inLen -1
                      then vLimit (subtract 1 . length . T.words $ ingr ^. entityName) vBorder
                      else emptyWidget
                )
      ]
   where
    missing = E.lookup ingr inv < n

  drawOut i (n, ingr) =
    hBox
      [ padRight (Pad 1) $
          ( joinableBorder (Edges (i /= 0) (i /= outLen -1) False True)
              <=> if i /= outLen -1
                then vLimit (subtract 1 . length . T.words $ ingr ^. entityName) vBorder
                else emptyWidget
          )
            <+> hBorder
      , fmtEntityName False ingr
      , padLeft (Pad 1) $ str (show n)
      ]

  -- If it's the focused entity, draw it highlighted.
  -- If the robot doesn't have any, draw it in red.
  fmtEntityName missing ingr
    | ingr == e = withAttr deviceAttr $ txtLines nm
    | ingr == timeE = withAttr sandAttr $ txtLines nm
    | missing = withAttr invalidFormInputAttr $ txtLines nm
    | otherwise = txtLines nm
   where
    -- Split up multi-word names, one line per word
    nm = ingr ^. entityName
    txtLines = vBox . map txt . T.words

-- | Ad-hoc entity to represent time - only used in recipe drawing
timeE :: Entity
timeE = mkEntity (defaultEntityDisplay '.') "ticks" [] []

drawReqs :: IngredientList Entity -> Widget Name
drawReqs = vBox . map (hCenter . drawReq)
 where
  drawReq (1, e) = txt $ e ^. entityName
  drawReq (n, e) = str (show n) <+> txt " " <+> txt (e ^. entityName)

indent2 :: WrapSettings
indent2 = defaultWrapSettings {fillStrategy = FillIndent 2}

drawRobotLog :: AppState -> Widget Name
drawRobotLog s =
  vBox
    [ padBottom (Pad 1) (hBorderWithLabel (txt "Log"))
    , vBox . imap drawEntry $ logEntries
    ]
 where
  logEntries = s ^. gameState . to focusedRobot . _Just . robotLog . to F.toList
  rn = s ^? gameState . to focusedRobot . _Just . robotName
  n = length logEntries

  allMe = all ((== rn) . Just . view leRobotName) logEntries

  drawEntry i e =
    (if i == n -1 && s ^. uiState . uiScrollToEnd then visible else id)
      . txtWrapWith indent2
      $ (if allMe then e ^. leText else T.concat ["[", e ^. leRobotName, "] ", e ^. leText])

------------------------------------------------------------
-- REPL panel
------------------------------------------------------------

-- | Draw the REPL.
drawREPL :: AppState -> Widget Name
drawREPL s =
  vBox $
    map fmt (getLatestREPLHistoryItems (replHeight - inputLines) history)
      ++ case isActive <$> base of
        Just False -> [renderForm (s ^. uiState . uiReplForm)]
        _ -> [padRight Max $ txt "..."]
      ++ [padRight Max $ txt histIdx | debugging]
 where
  debugging = False -- Turn ON to get extra line with history index
  inputLines = 1 + fromEnum debugging
  history = s ^. uiState . uiReplHistory
  base = s ^. gameState . robotMap . at "base"
  histIdx = fromString $ show (history ^. replIndex)
  fmt (REPLEntry e) = txt replPrompt <+> txt e
  fmt (REPLOutput t) = txt t
