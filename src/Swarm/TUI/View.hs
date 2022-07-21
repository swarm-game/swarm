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

  -- * Dialog box
  drawDialog,
  generateModal,
  chooseCursor,

  -- * Key hint menu
  drawKeyMenu,
  drawKeyCmd,

  -- * World
  drawWorld,

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

import Brick hiding (Direction)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Brick.Widgets.Dialog
import Brick.Widgets.List qualified as BL
import Brick.Widgets.Table qualified as BT
import Control.Lens hiding (Const, from)
import Data.Array (range)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable qualified as F
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Linear
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Scenario (ScenarioItem (..), scenarioDescription, scenarioItemName, scenarioName)
import Swarm.Game.State
import Swarm.Game.Terrain (terrainMap)
import Swarm.Game.World qualified as W
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Model
import Swarm.TUI.Panel
import Swarm.Util
import System.Clock (TimeSpec (..))
import Text.Printf
import Text.Wrap
import Witch (from)

-- | The main entry point for drawing the entire UI.  Figures out
--   which menu screen we should show (if any), or just the game itself.
drawUI :: AppState -> [Widget Name]
drawUI s
  | s ^. uiState . uiPlaying = drawGameUI s
  | otherwise = case s ^. uiState . uiMenu of
    -- We should never reach the NoMenu case if uiPlaying is false; we would have
    -- quit the app instead.  But just in case, we display the main menu anyway.
    NoMenu -> [drawMainMenuUI (s ^. uiState . appData . at "logo") (mainMenu NewGame)]
    MainMenu l -> [drawMainMenuUI (s ^. uiState . appData . at "logo") l]
    NewGameMenu stk -> [drawNewGameMenuUI stk]
    TutorialMenu -> [drawTutorialMenuUI]
    AboutMenu -> [drawAboutMenuUI (s ^. uiState . appData . at "about")]

drawMainMenuUI :: Maybe Text -> BL.List Name MainMenuEntry -> Widget Name
drawMainMenuUI logo l =
  vBox
    [ maybe emptyWidget drawLogo logo
    , centerLayer . vLimit 5 . hLimit 20 $
        BL.renderList (const (hCenter . drawMainMenuEntry)) True l
    ]

drawLogo :: Text -> Widget Name
drawLogo = centerLayer . vBox . map (hBox . T.foldr (\c ws -> drawThing c : ws) []) . T.lines
 where
  drawThing :: Char -> Widget Name
  drawThing c = withAttr (attrFor c) $ str [c]

  attrFor :: Char -> AttrName
  attrFor c
    | c `elem` ("<>v^" :: String) = robotAttr
  attrFor 'T' = plantAttr
  attrFor '@' = rockAttr
  attrFor '~' = waterAttr
  attrFor '▒' = dirtAttr
  attrFor _ = defAttr

drawNewGameMenuUI :: NonEmpty (BL.List Name ScenarioItem) -> Widget Name
drawNewGameMenuUI (l :| ls) =
  padLeftRight 20
    . centerLayer
    $ hBox
      [ vBox
          [ withAttr robotAttr . txt $ breadcrumbs ls
          , txt " "
          , vLimit 20 . hLimit 35
              . BL.renderList (const drawScenarioItem) True
              $ l
          ]
      , padLeft (Pad 5) (maybe (txt "") (drawDescription . snd) (BL.listSelectedElement l))
      ]
 where
  drawScenarioItem (SISingle s) = padRight Max . txt $ s ^. scenarioName
  drawScenarioItem (SICollection nm _) = padRight Max (txt nm) <+> withAttr robotAttr (txt ">")

  breadcrumbs :: [BL.List Name ScenarioItem] -> Text
  breadcrumbs =
    T.intercalate " > "
      . ("Scenarios" :)
      . reverse
      . mapMaybe (fmap (scenarioItemName . snd) . BL.listSelectedElement)

  drawDescription :: ScenarioItem -> Widget Name
  drawDescription (SISingle s) = txtWrap (nonBlank (s ^. scenarioDescription))
  drawDescription (SICollection _ _) = txtWrap " "

  nonBlank "" = " "
  nonBlank t = t

drawMainMenuEntry :: MainMenuEntry -> Widget Name
drawMainMenuEntry NewGame = txt "New game"
drawMainMenuEntry Tutorial = txt "Tutorial"
drawMainMenuEntry About = txt "About"
drawMainMenuEntry Quit = txt "Quit"

drawTutorialMenuUI :: Widget Name
drawTutorialMenuUI =
  centerLayer $
    vBox . map hCenter $
      [ txt "Coming soon! In the meantime, check out the tutorial at"
      , txt "https://github.com/swarm-game/swarm/blob/main/TUTORIAL.md ."
      , txt " "
      , txt "You can also play through a few in-progress"
      , txt "tutorial challenges over in the New Game menu."
      , txt " "
      , txt "https://github.com/swarm-game/swarm/issues/25"
      , txt "https://github.com/swarm-game/swarm/issues/296"
      ]

drawAboutMenuUI :: Maybe Text -> Widget Name
drawAboutMenuUI Nothing = centerLayer $ txt "About swarm!"
drawAboutMenuUI (Just t) = centerLayer . vBox . map (hCenter . txt . nonblank) $ T.lines t
 where
  -- Turn blank lines into a space so they will take up vertical space as widgets
  nonblank "" = " "
  nonblank s = s

-- | Draw the main game UI.  Generates a list of widgets, where each
--   represents a layer.  Right now we just generate two layers: the
--   main layer and a layer for a floating dialog that can be on top.
drawGameUI :: AppState -> [Widget Name]
drawGameUI s =
  [ drawDialog s
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
                ( plainBorder
                    & bottomLabels . rightLabel ?~ padLeftRight 1 (drawTPS s)
                    & addCursorPos
                    & addClock
                )
                (drawWorld $ s ^. gameState)
            , drawKeyMenu s
            , clickable REPLPanel $
                panel
                  highlightAttr
                  fr
                  REPLPanel
                  ( plainBorder
                      & topLabels . rightLabel .~ (drawType <$> (s ^. uiState . uiReplType))
                  )
                  ( vLimit replHeight
                      . padBottom Max
                      . padLeftRight 1
                      $ drawREPL s
                  )
            ]
        ]
  ]
 where
  addCursorPos = case s ^. uiState . uiWorldCursor of
    Just coord -> bottomLabels . leftLabel ?~ padLeftRight 1 (drawWorldCursorInfo (s ^. gameState) coord)
    Nothing -> id
  -- Add clock display in top right of the world view if focused robot
  -- has a clock installed
  addClock = topLabels . rightLabel ?~ padLeftRight 1 (drawClockDisplay s)
  fr = s ^. uiState . uiFocusRing
  moreTop = s ^. uiState . uiMoreInfoTop
  moreBot = s ^. uiState . uiMoreInfoBot

drawWorldCursorInfo :: GameState -> W.Coords -> Widget Name
drawWorldCursorInfo g i@(W.Coords (y, x)) =
  hBox [drawLoc g i, txt $ " at " <> from (show x) <> " " <> from (show (y * (-1)))]

drawClockDisplay :: AppState -> Widget n
drawClockDisplay s
  | clockInstalled && gamePaused = clockWidget <+> txt " " <+> pauseWidget
  | clockInstalled = clockWidget
  | gamePaused = pauseWidget
  | otherwise = txt ""
 where
  clockInstalled = case s ^. gameState . to focusedRobot of
    Nothing -> False
    Just r
      | countByName "clock" (r ^. installedDevices) > 0 -> True
      | otherwise -> False
  gamePaused = s ^. gameState . paused
  clockWidget = drawClock (s ^. gameState . ticks) gamePaused
  pauseWidget = txt "(PAUSED)"

drawClock :: Integer -> Bool -> Widget n
drawClock t showTicks =
  str . mconcat $
    [ printf "%x" (t `shiftR` 20)
    , ":"
    , printf "%02x" ((t `shiftR` 12) .&. ((1 `shiftL` 8) - 1))
    , ":"
    , printf "%02x" ((t `shiftR` 4) .&. ((1 `shiftL` 8) - 1))
    ]
      ++ if showTicks then [".", printf "%x" (t .&. ((1 `shiftL` 4) - 1))] else []

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

-- | Width cap for modal and error message windows
maxModalWindowWidth :: Int
maxModalWindowWidth = 500

-- | Render the error dialog window with a given error message
renderErrorDialog :: Text -> Widget Name
renderErrorDialog err = renderDialog (dialog (Just "Error") Nothing (maxModalWindowWidth `min` requiredWidth)) errContent
 where
  errContent = txtWrapWith indent2 {preserveIndentation = True} err
  requiredWidth = 2 + maximum (textWidth <$> T.lines err)

-- | Draw the error dialog window, if it should be displayed right now.
drawDialog :: AppState -> Widget Name
drawDialog s = case s ^. uiState . uiModal of
  Just (Modal mt d) -> renderDialog d (withVScrollBars OnRight $ drawModal s mt)
  Nothing -> maybe emptyWidget renderErrorDialog (s ^. uiState . uiError)

drawModal :: AppState -> ModalType -> Widget Name
drawModal s = \case
  HelpModal -> helpWidget
  RobotsModal -> robotsListWidget s
  RecipesModal -> availableListWidget (s ^. gameState) RecipeList
  CommandsModal -> availableListWidget (s ^. gameState) CommandList
  MessagesModal -> availableListWidget (s ^. gameState) MessageList
  WinModal -> padBottom (Pad 1) $ hCenter $ txt "Congratulations!"
  DescriptionModal e -> descriptionWidget s e
  QuitModal -> padBottom (Pad 1) $ hCenter $ txt (quitMsg (s ^. uiState . uiMenu))
  GoalModal g -> padLeftRight 1 (displayParagraphs g)

quitMsg :: Menu -> Text
quitMsg m = "Are you sure you want to " <> quitAction <> "? All progress will be lost!"
 where
  quitAction = case m of
    NoMenu -> "quit"
    _ -> "quit and return to the menu"

-- | Generate a fresh modal window of the requested type.
generateModal :: AppState -> ModalType -> Modal
generateModal s mt = Modal mt (dialog (Just title) buttons (maxModalWindowWidth `min` requiredWidth))
 where
  haltingMessage = case s ^. uiState . uiMenu of
    NoMenu -> Just "Quit"
    _ -> Nothing
  descriptionWidth = 100
  (title, buttons, requiredWidth) =
    case mt of
      HelpModal -> (" Help ", Nothing, maxModalWindowWidth)
      RobotsModal -> ("Robots", Nothing, descriptionWidth)
      RecipesModal -> ("Available Recipes", Nothing, descriptionWidth)
      CommandsModal -> ("Available Commands", Nothing, descriptionWidth)
      MessagesModal -> ("Messages", Just (0, [("Toggle pause", PauseButton)]), descriptionWidth)
      WinModal ->
        let nextMsg = "Next challenge!"
            stopMsg = fromMaybe "Return to the menu" haltingMessage
            continueMsg = "Keep playing"
         in ( ""
            , Just
                ( 0
                , [(nextMsg, NextButton scene) | Just scene <- [s ^. uiState . uiNextScenario]]
                  ++ [ (stopMsg, QuitButton)
                     , (continueMsg, CancelButton)
                     ]
                )
            , sum (map length [nextMsg, stopMsg, continueMsg]) + 32
            )
      DescriptionModal e -> (descriptionTitle e, Nothing, descriptionWidth)
      QuitModal ->
        let stopMsg = fromMaybe "Quit to menu" haltingMessage
         in ( ""
            , Just (0, [("Keep playing", CancelButton), (stopMsg, QuitButton)])
            , T.length (quitMsg (s ^. uiState . uiMenu)) + 4
            )
      GoalModal _ -> (" Goal ", Nothing, 80)

robotsListWidget :: AppState -> Widget Name
robotsListWidget s = viewport RobotsViewport Vertical (hCenter table)
 where
  table =
    BT.renderTable
      . BT.columnBorders False
      . BT.setDefaultColAlignment BT.AlignCenter
      -- Inventory count is right aligned
      . BT.alignRight 4
      . BT.table
      $ map (padLeftRight 1) <$> (headers : robotsTable)
  headers =
    withAttr robotAttr
      <$> [ txt "Name"
          , txt "Age"
          , txt "Position"
          , txt "Inventory"
          , txt "Status"
          , txt "Log"
          ]
  robotsTable = mkRobotRow <$> robots
  mkRobotRow robot =
    [ nameWidget
    , txt $ from ageStr
    , locWidget
    , padRight (Pad 1) (txt $ from $ show rInvCount)
    , statusWidget
    , txt rLog
    ]
   where
    nameWidget = hBox [displayEntity (robot ^. robotEntity), higlightSystem . txt $ " " <> robot ^. robotName]
    higlightSystem = if robot ^. systemRobot then withAttr highlightAttr else id

    ageStr
      | age < 60 = show age <> "sec"
      | age < 3600 = show (age `div` 60) <> "min"
      | age < 3600 * 24 = show (age `div` 3600) <> "hour"
      | otherwise = show (age `div` 3600 * 24) <> "day"
     where
      TimeSpec createdAtSec _ = robot ^. robotCreatedAt
      TimeSpec nowSec _ = s ^. uiState . lastFrameTime
      age = nowSec - createdAtSec

    rInvCount = sum $ map fst . E.elems $ robot ^. robotEntity . entityInventory
    rLog
      | robot ^. robotLogUpdated = "x"
      | otherwise = " "

    locWidget = hBox [worldCell, txt $ " " <> locStr]
     where
      rloc@(V2 x y) = robot ^. robotLocation
      worldCell = drawLoc g (W.locToCoords rloc)
      locStr = from (show x) <> " " <> from (show y)

    statusWidget = case robot ^. machine of
      Waiting {} -> txt "waiting"
      _
        | isActive robot -> withAttr notifAttr $ txt "busy"
        | otherwise -> withAttr greenAttr $ txt "idle"

  basePos :: V2 Double
  basePos = realToFrac <$> fromMaybe (V2 0 0) (g ^? robotMap . ix 0 . robotLocation)
  -- Keep the base and non sytem robot (e.g. no seed)
  isRelevant robot = robot ^. robotID == 0 || not (robot ^. systemRobot)
  -- Keep the robot that are less than 32 unit away from the base
  isNear robot = creative || distance (realToFrac <$> robot ^. robotLocation) basePos < 32
  robots :: [Robot]
  robots =
    filter (\robot -> debugging || (isRelevant robot && isNear robot))
      . IM.elems
      $ g ^. robotMap
  creative = g ^. creativeMode
  cheat = s ^. uiState . uiCheatMode
  debugging = creative && cheat
  g = s ^. gameState

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
    , ("F2", "Robots list")
    , ("F3", "Available recipes")
    , ("F4", "Available commands")
    , ("F5", "Messages")
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
    [ ("build {<commands>}", "Create a robot")
    , ("make \"<name>\"", "Craft an item")
    , ("move", "Move one step in the current direction")
    , ("turn <dir>", "Change the current direction")
    , ("grab", "Grab whatver is available")
    , ("give <robot> \"<item>\"", "Give an item to another robot")
    , ("has \"<item>\"", "Check for an item in the inventory")
    ]

data NotificationList = RecipeList | CommandList | MessageList

availableListWidget :: GameState -> NotificationList -> Widget Name
availableListWidget gs nl = viewport vp Vertical (padTop (Pad 1) $ vBox widgetList)
 where
  (vp, widgetList) = case nl of
    RecipeList -> (RecipesViewport, mkAvailableList gs availableRecipes renderRecipe)
    CommandList -> (CommandsViewport, mkAvailableList gs availableCommands renderCommand & (<> constWiki) . (padLeftRight 18 constHeader :))
    MessageList -> (MessageViewport, messagesWidget gs)
  renderRecipe = padLeftRight 18 . drawRecipe Nothing (fromMaybe E.empty inv)
  inv = gs ^? to focusedRobot . _Just . robotInventory
  renderCommand = padLeftRight 18 . drawConst

mkAvailableList :: GameState -> Lens' GameState (Notifications a) -> (a -> Widget Name) -> [Widget Name]
mkAvailableList gs notifLens notifRender = map padRender news <> notifSep <> map padRender knowns
 where
  padRender = padBottom (Pad 1) . notifRender
  count = gs ^. notifLens . notificationsCount
  (news, knowns) = splitAt count (gs ^. notifLens . notificationsContent)
  notifSep
    | count > 0 && not (null knowns) =
      [ padBottom (Pad 1) (withAttr redAttr $ hBorderWithLabel (padLeftRight 1 (txt "new↑")))
      ]
    | otherwise = []

constHeader :: Widget Name
constHeader = padBottom (Pad 1) $ withAttr robotAttr $ padLeft (Pad 1) $ txt "command name : type"

constWiki :: [Widget Name]
constWiki =
  padLeftRight 13
    <$> [ padTop (Pad 2) $ txt "For the full list of available commands see the Wiki at:"
        , txt "https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet"
        ]

drawConst :: Const -> Widget Name
drawConst c = hBox [padLeft (Pad $ 13 - T.length constName) (txt constName), txt constSig]
 where
  constName = syntax . constInfo $ c
  constSig = " : " <> prettyText (inferConst c)

descriptionTitle :: Entity -> String
descriptionTitle e = " " ++ from @Text (e ^. entityName) ++ " "

-- | Generate a pop-up widget to display the description of an entity.
descriptionWidget :: AppState -> Entity -> Widget Name
descriptionWidget s e = padLeftRight 1 (explainEntry s e)

-- | Draw a widget with messages to the current robot.
messagesWidget :: GameState -> [Widget Name]
messagesWidget gs = widgetList
 where
  widgetList = focusNewest . map drawLogEntry' $ gs ^. messageNotifications . notificationsContent
  focusNewest = if gs ^. paused then id else over _last visible
  drawLogEntry' (e, isLog) =
    withAttr (if isLog then notifAttr else robotColor (e ^. leRobotID)) $
      hBox
        [ txt $ "[" <> view leRobotName e <> "] "
        , txtWrapWith indent2 (e ^. leText)
        ]
  -- color each robot message with different color of the world (except those with background)
  robotColor rid = fgCols !! (rid `mod` fgColLen)
  fgCols = filter (`notElem` [waterAttr, iceAttr]) worldAttributes
  fgColLen = length fgCols

-- | Draw a menu explaining what key commands are available for the
--   current panel.  This menu is displayed as a single line in
--   between the world panel and the REPL.
drawKeyMenu :: AppState -> Widget Name
drawKeyMenu s =
  vLimit 1
    . hBox
    . (++ [gameModeWidget])
    . map (padLeftRight 1 . drawKeyCmd)
    . (globalKeyCmds ++)
    . map (\(k, n) -> (NoHighlight, k, n))
    . keyCmdsFor
    . focusGetCurrent
    . view (uiState . uiFocusRing)
    $ s
 where
  isReplWorking = s ^. gameState . replWorking
  isPaused = s ^. gameState . paused
  viewingBase = (s ^. gameState . viewCenterRule) == VCRobot 0
  creative = s ^. gameState . creativeMode
  cheat = s ^. uiState . uiCheatMode
  goal = (s ^. uiState . uiGoal) /= NoGoal

  notificationKey :: Getter GameState (Notifications a) -> Text -> Text -> Maybe (KeyHighlight, Text, Text)
  notificationKey notifLens key name
    | null (s ^. gameState . notifLens . notificationsContent) = Nothing
    | otherwise =
      let highlight
            | s ^. gameState . notifLens . notificationsCount > 0 = Highlighted
            | otherwise = NoHighlight
       in Just (highlight, key, name)

  gameModeWidget =
    padLeft Max . padLeftRight 1
      . txt
      . (<> " mode")
      $ case creative of
        False -> "Classic"
        True -> "Creative"
  globalKeyCmds =
    catMaybes
      [ Just (NoHighlight, "F1", "help")
      , Just (NoHighlight, "F2", "robots")
      , notificationKey availableRecipes "F3" "Recipes"
      , notificationKey availableCommands "F4" "Commands"
      , notificationKey messageNotifications "F5" "Messages"
      , may cheat (NoHighlight, "^v", "creative")
      , may goal (NoHighlight, "^g", "goal")
      ]
  may b = if b then Just else const Nothing

  keyCmdsFor (Just REPLPanel) =
    [ ("↓↑", "history")
    ]
      ++ [("Ret", "execute") | not isReplWorking]
      ++ [("^c", "cancel") | isReplWorking]
  keyCmdsFor (Just WorldPanel) =
    [ ("←↓↑→ / hjkl", "scroll") | creative
    ]
      ++ [ ("<>", "slower/faster")
         , ("p", if isPaused then "unpause" else "pause")
         ]
      ++ [("s", "step") | isPaused]
      ++ [("c", "recenter") | not viewingBase]
  keyCmdsFor (Just RobotPanel) =
    [ ("↓↑/Pg{Up,Dn}/Home/End/jk", "navigate")
    , ("Ret", "focus")
    , ("m", "make")
    , ("0", "hide/show 0")
    ]
  keyCmdsFor (Just InfoPanel) =
    [ ("↓↑/Pg{Up,Dn}/Home/End/jk", "scroll")
    ]
  keyCmdsFor _ = []

data KeyHighlight = NoHighlight | Highlighted

-- | Draw a single key command in the menu.
drawKeyCmd :: (KeyHighlight, Text, Text) -> Widget Name
drawKeyCmd (Highlighted, key, cmd) = hBox [withAttr notifAttr (txt $ T.concat ["[", key, "] "]), txt cmd]
drawKeyCmd (NoHighlight, key, cmd) = txt $ T.concat ["[", key, "] ", cmd]

------------------------------------------------------------
-- World panel
------------------------------------------------------------

-- | Draw the current world view.
drawWorld :: GameState -> Widget Name
drawWorld g =
  center
    . cached WorldCache
    . reportExtent WorldExtent
    -- Set the clickable request after the extent to play nice with the cache
    . clickable WorldPanel
    . Widget Fixed Fixed
    $ do
      ctx <- getContext
      let w = ctx ^. availWidthL
          h = ctx ^. availHeightL
          ixs = range (viewingRegion g (fromIntegral w, fromIntegral h))
      render . vBox . map hBox . chunksOf w . map (drawLoc g) $ ixs

-- | Render the 'Display' for a specific location.
drawLoc :: GameState -> W.Coords -> Widget Name
drawLoc g = renderDisplay . displayLoc g

-- | Get the 'Display' for a specific location, by combining the
--   'Display's for the terrain, entity, and robots at the location.
displayLoc :: GameState -> W.Coords -> Display
displayLoc g coords =
  sconcat . NE.fromList $
    [terrainMap M.! toEnum (W.lookupTerrain coords (g ^. world))]
      ++ maybeToList (displayForEntity <$> W.lookupEntity coords (g ^. world))
      ++ map (view robotDisplay) (robotsAtLocation (W.coordsToLoc coords) g)
 where
  displayForEntity :: Entity -> Display
  displayForEntity e = (if known e then id else hidden) (e ^. entityDisplay)

  known e =
    e `hasProperty` Known
      || case hidingMode g of
        HideAllEntities -> False
        HideNoEntity -> True
        HideEntityUnknownTo ro -> ro `robotKnows` e

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

hidingMode :: GameState -> HideEntity
hidingMode g
  | g ^. creativeMode = HideNoEntity
  | otherwise = maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g

------------------------------------------------------------
-- Robot inventory panel
------------------------------------------------------------

-- | Draw info about the currently focused robot, such as its name,
--   position, orientation, and inventory.
drawRobotPanel :: AppState -> Widget Name
drawRobotPanel s = case (s ^. gameState . to focusedRobot, s ^. uiState . uiInventory) of
  (Just r, Just (_, lst)) ->
    let V2 x y = r ^. robotLocation
        drawClickableItem pos selb = clickable (InventoryListItem pos) . drawItem (lst ^. BL.listSelectedL) pos selb
     in padBottom Max $
          vBox
            [ hCenter $
                hBox
                  [ txt (r ^. robotName)
                  , padLeft (Pad 2) $ str (printf "(%d, %d)" x y)
                  , padLeft (Pad 2) $ displayEntity (r ^. robotEntity)
                  ]
            , padAll 1 (BL.renderListWithIndex drawClickableItem True lst)
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
explainFocusedItem s = case focusedItem s of
  Just (InventoryEntry _ e) -> explainEntry s e
  Just (InstalledEntry e) ->
    explainEntry s e
      -- Special case: installed logger device displays the robot's log.
      <=> if e ^. entityName == "logger" then drawRobotLog s else emptyWidget
  _ -> txt " "

explainEntry :: AppState -> Entity -> Widget Name
explainEntry s e =
  vBox
    [ displayProperties (e ^. entityProperties)
    , displayParagraphs (e ^. entityDescription)
    , explainRecipes s e
    ]

displayProperties :: [EntityProperty] -> Widget Name
displayProperties = displayList . mapMaybe showProperty
 where
  showProperty Growable = Just "growing"
  showProperty Infinite = Just "infinite"
  showProperty Liquid = Just "liquid"
  showProperty Unwalkable = Just "blocking"
  -- Most things are portable so we don't show that.
  showProperty Portable = Nothing
  -- 'Known' is just a technical detail of how we handle some entities
  -- in challenge scenarios and not really something the player needs
  -- to know.
  showProperty Known = Nothing

  displayList [] = emptyWidget
  displayList ps =
    vBox
      [ hBox . L.intersperse (txt ", ") . map (withAttr robotAttr . txt) $ ps
      , txt " "
      ]

explainRecipes :: AppState -> Entity -> Widget Name
explainRecipes s e
  | null recipes = emptyWidget
  | otherwise =
    vBox
      [ padBottom (Pad 1) (hBorderWithLabel (txt "Recipes"))
      , padLeftRight 2 $
          hCenter $
            vBox $
              map (hLimit widthLimit . padBottom (Pad 1) . drawRecipe (Just e) inv) recipes
      ]
 where
  recipes = recipesWith s e

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

recipesWith :: AppState -> Entity -> [Recipe Entity]
recipesWith s e =
  let getRecipes select = recipesFor (s ^. gameState . select) e
   in L.nub $ getRecipes recipesOut ++ getRecipes recipesIn

-- | Draw an ASCII art representation of a recipe.  For now, the
--   weight is not shown.
drawRecipe :: Maybe Entity -> Inventory -> Recipe Entity -> Widget Name
drawRecipe me inv (Recipe ins outs reqs time _weight) =
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
            <+> ( joinableBorder (Edges (i /= 0) (i /= inLen - 1) True False) -- ...maybe plus vert ext:   │
                    <=> if i /= inLen - 1
                      then vLimit (subtract 1 . length . T.words $ ingr ^. entityName) vBorder
                      else emptyWidget
                )
      ]
   where
    missing = E.lookup ingr inv < n

  drawOut i (n, ingr) =
    hBox
      [ padRight (Pad 1) $
          ( joinableBorder (Edges (i /= 0) (i /= outLen - 1) False True)
              <=> if i /= outLen - 1
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
    | Just ingr == me = withAttr deviceAttr $ txtLines nm
    | ingr == timeE = withAttr sandAttr $ txtLines nm
    | missing = withAttr invalidFormInputAttr $ txtLines nm
    | otherwise = txtLines nm
   where
    -- Split up multi-word names, one line per word
    nm = ingr ^. entityName
    txtLines = vBox . map txt . T.words

-- | Ad-hoc entity to represent time - only used in recipe drawing
timeE :: Entity
timeE = mkEntity (defaultEntityDisplay '.') "ticks" [] [] []

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
    (if i == n - 1 && s ^. uiState . uiScrollToEnd then visible else id) $
      drawLogEntry (not allMe) e

-- | Draw one log entry with an optional robot name first.
drawLogEntry :: Bool -> LogEntry -> Widget a
drawLogEntry addName e = txtWrapWith indent2 . (if addName then (name <>) else id) $ e ^. leText
 where
  name = "[" <> view leRobotName e <> "] "

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
  base = s ^. gameState . robotMap . at 0
  histIdx = fromString $ show (history ^. replIndex)
  fmt (REPLEntry e) = txt $ "> " <> e
  fmt (REPLOutput t) = txt t

------------------------------------------------------------
-- Utility
------------------------------------------------------------

-- | Display a list of text-wrapped paragraphs with one blank line after
--   each.
displayParagraphs :: [Text] -> Widget Name
displayParagraphs = vBox . map (padBottom (Pad 1) . txtWrap)
