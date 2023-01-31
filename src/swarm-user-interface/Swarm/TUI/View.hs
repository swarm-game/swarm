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
  drawModalMenu,
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

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (getEditContents, renderEditor)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.Table qualified as BT
import Control.Lens hiding (Const, from)
import Control.Monad (guard)
import Data.Array (range)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable qualified as F
import Data.Functor (($>))
import Data.IntMap qualified as IM
import Data.List (intersperse)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Linear
import Linear.Affine (Point)
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Scenario (scenarioAuthor, scenarioDescription, scenarioName, scenarioObjectives)
import Swarm.TUI.Model.Goal (goalsContent, hasAnythingToShow)
import Swarm.TUI.View.Objective qualified as GR
import Swarm.Game.ScenarioInfo (
  ScenarioItem (..),
  ScenarioStatus (..),
  scenarioBestTicks,
  scenarioBestTime,
  scenarioItemName,
  scenarioStatus,
 )
import Swarm.Game.State
import Swarm.Game.World qualified as W
import Swarm.Language.Capability (constCaps)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Inventory.Sorting (renderSortMethod)
import Swarm.TUI.Model
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.Panel
import Swarm.TUI.View.Achievement
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Util
import Swarm.Util.Util
import Swarm.Game.Location
import Swarm.Version (NewReleaseFailure (..))
import System.Clock (TimeSpec (..))
import Text.Printf
import Text.Wrap
import Witch (from, into)

-- | The main entry point for drawing the entire UI.  Figures out
--   which menu screen we should show (if any), or just the game itself.
drawUI :: AppState -> [Widget Name]
drawUI s
  | s ^. uiState . uiPlaying = drawGameUI s
  | otherwise = case s ^. uiState . uiMenu of
      -- We should never reach the NoMenu case if uiPlaying is false; we would have
      -- quit the app instead.  But just in case, we display the main menu anyway.
      NoMenu -> [drawMainMenuUI s (mainMenu NewGame)]
      MainMenu l -> [drawMainMenuUI s l]
      NewGameMenu stk -> [drawNewGameMenuUI stk]
      AchievementsMenu l -> [drawAchievementsMenuUI s l]
      MessagesMenu -> [drawMainMessages s]
      AboutMenu -> [drawAboutMenuUI (s ^. uiState . appData . at "about")]

drawMainMessages :: AppState -> Widget Name
drawMainMessages s = renderDialog dial . padBottom Max . scrollList $ drawLogs ls
 where
  ls = reverse $ s ^. runtimeState . eventLog . notificationsContent
  dial = dialog (Just $ str "Messages") Nothing maxModalWindowWidth
  scrollList = withVScrollBars OnRight . vBox
  drawLogs = map (drawLogEntry True)

drawMainMenuUI :: AppState -> BL.List Name MainMenuEntry -> Widget Name
drawMainMenuUI s l =
  vBox . catMaybes $
    [ drawLogo <$> logo
    , hCenter . padTopBottom 2 <$> newVersionWidget version
    , Just . centerLayer . vLimit 6 . hLimit 20 $
        BL.renderList (const (hCenter . drawMainMenuEntry s)) True l
    ]
 where
  logo = s ^. uiState . appData . at "logo"
  version = s ^. runtimeState . upstreamRelease

newVersionWidget :: Either NewReleaseFailure String -> Maybe (Widget n)
newVersionWidget = \case
  Right ver -> Just . txt $ "New version " <> T.pack ver <> " is available!"
  Left (OnDevelopmentBranch _b) -> Just . txt $ "Good luck developing!"
  Left (FailedReleaseQuery _f) -> Nothing
  Left (NoMainUpstreamRelease _fails) -> Nothing
  Left (OldUpstreamRelease _up _my) -> Nothing

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
          [ withAttr boldAttr . txt $ breadcrumbs ls
          , txt " "
          , vLimit 20
              . hLimit 35
              . BL.renderList (const $ padRight Max . drawScenarioItem) True
              $ l
          ]
      , padLeft (Pad 5) (maybe (txt "") (drawDescription . snd) (BL.listSelectedElement l))
      ]
 where
  drawScenarioItem (SISingle (s, si)) = padRight (Pad 1) (drawStatusInfo s si) <+> txt (s ^. scenarioName)
  drawScenarioItem (SICollection nm _) = padRight (Pad 1) (withAttr boldAttr $ txt " > ") <+> txt nm
  drawStatusInfo s si = case si ^. scenarioBestTime of
    NotStarted -> txt " ○ "
    InProgress {} -> case s ^. scenarioObjectives of
      [] -> withAttr cyanAttr $ txt " ◉ "
      _ -> withAttr yellowAttr $ txt " ◎ "
    Complete {} -> withAttr greenAttr $ txt " ● "

  describeStatus = \case
    NotStarted -> txt "none"
    InProgress _s e _t ->
      withAttr yellowAttr . vBox $
        [ txt "in progress"
        , txt $ "(played for " <> formatTimeDiff e <> ")"
        ]
    Complete _s e t ->
      withAttr greenAttr . vBox $
        [ txt $ "completed in " <> formatTimeDiff e
        , hBox
            [ txt "("
            , drawTime t True
            , txt " ticks)"
            ]
        ]

  formatTimeDiff :: NominalDiffTime -> Text
  formatTimeDiff = T.pack . formatTime defaultTimeLocale "%hh %Mm %Ss"

  breadcrumbs :: [BL.List Name ScenarioItem] -> Text
  breadcrumbs =
    T.intercalate " > "
      . ("Scenarios" :)
      . reverse
      . mapMaybe (fmap (scenarioItemName . snd) . BL.listSelectedElement)

  drawDescription :: ScenarioItem -> Widget Name
  drawDescription (SICollection _ _) = txtWrap " "
  drawDescription (SISingle (s, si)) = do
    let oneBest = si ^. scenarioBestTime == si ^. scenarioBestTicks
    let bestRealTime = if oneBest then "best:" else "best real time:"
    let noSame = if oneBest then const Nothing else Just
    let lastText = let la = "last:" in padRight (Pad $ T.length bestRealTime - T.length la) (txt la)
    vBox . catMaybes $
      [ Just $ txtWrap (nonBlank (s ^. scenarioDescription))
      , padTop (Pad 1)
          . withAttr dimAttr
          . (txt "Author: " <+>)
          . txt
          <$> (s ^. scenarioAuthor)
      , Just $
          padTop (Pad 3) $
            padRight (Pad 1) (txt bestRealTime) <+> describeStatus (si ^. scenarioBestTime)
      , noSame $ -- hide best game time if it is same as best real time
          padTop (Pad 1) $
            txt "best game time: " <+> describeStatus (si ^. scenarioBestTicks)
      , Just $
          padTop (Pad 1) $
            padRight (Pad 1) lastText <+> describeStatus (si ^. scenarioStatus)
      ]

  nonBlank "" = " "
  nonBlank t = t

drawMainMenuEntry :: AppState -> MainMenuEntry -> Widget Name
drawMainMenuEntry s = \case
  NewGame -> txt "New game"
  Tutorial -> txt "Tutorial"
  Achievements -> txt "Achievements"
  About -> txt "About"
  Messages -> highlightMessages $ txt "Messages"
  Quit -> txt "Quit"
 where
  highlightMessages =
    if s ^. runtimeState . eventLog . notificationsCount > 0
      then withAttr notifAttr
      else id

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
  [ joinBorders $ drawDialog s
  , joinBorders $
      hBox
        [ hLimitPercent 25 $
            vBox
              [ vLimitPercent 50 $ panel highlightAttr fr (FocusablePanel RobotPanel) plainBorder $ drawRobotPanel s
              , panel
                  highlightAttr
                  fr
                  (FocusablePanel InfoPanel)
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
                (FocusablePanel WorldPanel)
                ( plainBorder
                    & bottomLabels . rightLabel ?~ padLeftRight 1 (drawTPS s)
                    & topLabels . leftLabel ?~ drawModalMenu s
                    & addCursorPos
                    & addClock
                )
                (drawWorld (s ^. uiState . uiShowRobots) (s ^. gameState))
            , drawKeyMenu s
            , clickable (FocusablePanel REPLPanel) $
                panel
                  highlightAttr
                  fr
                  (FocusablePanel REPLPanel)
                  ( plainBorder
                      & topLabels . rightLabel .~ (drawType <$> (s ^. uiState . uiREPL . replType))
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
    Nothing -> id
    Just coord ->
      let worldCursorInfo = drawWorldCursorInfo (s ^. gameState) coord
       in bottomLabels . leftLabel ?~ padLeftRight 1 worldCursorInfo
  -- Add clock display in top right of the world view if focused robot
  -- has a clock equipped
  addClock = topLabels . rightLabel ?~ padLeftRight 1 (drawClockDisplay $ s ^. gameState)
  fr = s ^. uiState . uiFocusRing
  moreTop = s ^. uiState . uiMoreInfoTop
  moreBot = s ^. uiState . uiMoreInfoBot

drawWorldCursorInfo :: GameState -> W.Coords -> Widget Name
drawWorldCursorInfo g coords@(W.Coords (y, x)) =
  hBox $ tileMemberWidgets ++ [coordsWidget]
 where
  coordsWidget =
    txt $
      T.unwords
        [ from $ show x
        , from $ show $ y * (-1)
        ]

  tileMembers = terrain : mapMaybe merge [entity, robot]
  tileMemberWidgets =
    map (padRight $ Pad 1) $
      concat $
        reverse $
          zipWith f tileMembers ["at", "on", "with"]
   where
    f cell preposition = [renderDisplay cell, txt preposition]

  terrain = displayTerrainCell g coords
  entity = displayEntityCell g coords
  robot = displayRobotCell g coords

  merge = fmap sconcat . NE.nonEmpty . filter (not . (^. invisible))

-- | Format the clock display to be shown in the upper right of the
--   world panel.
drawClockDisplay :: GameState -> Widget n
drawClockDisplay gs = hBox . intersperse (txt " ") $ catMaybes [clockWidget, pauseWidget]
 where
  clockWidget = maybeDrawTime (gs ^. ticks) (gs ^. paused) gs
  pauseWidget = guard (gs ^. paused) $> txt "(PAUSED)"

-- | Check whether the currently focused robot (if any) has a clock
--   device equipped.
clockEquipped :: GameState -> Bool
clockEquipped gs = case focusedRobot gs of
  Nothing -> False
  Just r
    | countByName "clock" (r ^. equippedDevices) > 0 -> True
    | otherwise -> False

-- | Format a ticks count as a hexadecimal clock.
drawTime :: Integer -> Bool -> Widget n
drawTime t showTicks =
  str . mconcat $
    [ printf "%x" (t `shiftR` 20)
    , ":"
    , printf "%02x" ((t `shiftR` 12) .&. ((1 `shiftL` 8) - 1))
    , ":"
    , printf "%02x" ((t `shiftR` 4) .&. ((1 `shiftL` 8) - 1))
    ]
      ++ if showTicks then [".", printf "%x" (t .&. ((1 `shiftL` 4) - 1))] else []

-- | Return a possible time display, if the currently focused robot
--   has a clock device equipped.  The first argument is the number
--   of ticks (e.g. 943 = 0x3af), and the second argument indicates
--   whether the time should be shown down to single-tick resolution
--   (e.g. 0:00:3a.f) or not (e.g. 0:00:3a).
maybeDrawTime :: Integer -> Bool -> GameState -> Maybe (Widget n)
maybeDrawTime t showTicks gs = guard (clockEquipped gs) $> drawTime t showTicks

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

-- | Render the error dialog window with a given error message
renderErrorDialog :: Text -> Widget Name
renderErrorDialog err = renderDialog (dialog (Just $ str "Error") Nothing (maxModalWindowWidth `min` requiredWidth)) errContent
 where
  errContent = txtWrapWith indent2 {preserveIndentation = True} err
  requiredWidth = 2 + maximum (textWidth <$> T.lines err)

-- | Draw the error dialog window, if it should be displayed right now.
drawDialog :: AppState -> Widget Name
drawDialog s = case s ^. uiState . uiModal of
  Just (Modal mt d) -> renderDialog d $ case mt of
    GoalModal -> drawModal s mt
    _ -> maybeScroll ModalViewport $ drawModal s mt
  Nothing -> maybe emptyWidget renderErrorDialog (s ^. uiState . uiError)

-- | Draw one of the various types of modal dialog.
drawModal :: AppState -> ModalType -> Widget Name
drawModal s = \case
  HelpModal -> helpWidget (s ^. gameState . seed) (s ^. runtimeState . webPort)
  RobotsModal -> robotsListWidget s
  RecipesModal -> availableListWidget (s ^. gameState) RecipeList
  CommandsModal -> commandsListWidget (s ^. gameState)
  MessagesModal -> availableListWidget (s ^. gameState) MessageList
  WinModal -> padBottom (Pad 1) $ hCenter $ txt "Congratulations!"
  LoseModal ->
    padBottom (Pad 1) $
      vBox $
        map
          (hCenter . txt)
          [ "Condolences!"
          , "This scenario is no longer winnable."
          ]
  DescriptionModal e -> descriptionWidget s e
  QuitModal -> padBottom (Pad 1) $ hCenter $ txt (quitMsg (s ^. uiState . uiMenu))
  GoalModal -> GR.renderGoalsDisplay (s ^. uiState . uiGoal)
  KeepPlayingModal -> padLeftRight 1 (displayParagraphs ["Have fun!  Hit Ctrl-Q whenever you're ready to proceed to the next challenge or return to the menu."])

robotsListWidget :: AppState -> Widget Name
robotsListWidget s = hCenter table
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
    nameWidget = hBox [renderDisplay (robot ^. robotDisplay), higlightSystem . txt $ " " <> robot ^. robotName]
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
      rloc@(Location x y) = robot ^. robotLocation
      worldCell = drawLoc (s ^. uiState . uiShowRobots) g (W.locToCoords rloc)
      locStr = from (show x) <> " " <> from (show y)

    statusWidget = case robot ^. machine of
      Waiting {} -> txt "waiting"
      _
        | isActive robot -> withAttr notifAttr $ txt "busy"
        | otherwise -> withAttr greenAttr $ txt "idle"

  basePos :: Point V2 Double
  basePos = realToFrac <$> fromMaybe origin (g ^? baseRobot . robotLocation)
  -- Keep the base and non system robot (e.g. no seed)
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

helpWidget :: Seed -> Maybe Port -> Widget Name
helpWidget theSeed mport =
  padTop (Pad 1) $
    (hBox . map (padLeftRight 2) $ [helpKeys, info])
      <=> padTop (Pad 1) (hCenter tips)
 where
  tips =
    vBox
      [ txt "Have questions? Want some tips? Check out:"
      , txt " "
      , txt "  - The Swarm wiki, https://github.com/swarm-game/swarm/wiki"
      , txt "  - The #swarm IRC channel on Libera.Chat"
      ]
  info =
    vBox
      [ txt "Configuration"
      , txt " "
      , txt ("Seed: " <> into @Text (show theSeed))
      , txt ("Web server port: " <> maybe "none" (into @Text . show) mport)
      ]
  helpKeys =
    vBox
      [ txt "Keybindings"
      , txt " "
      , mkTable glKeyBindings
      ]
  mkTable =
    BT.renderTable
      . BT.surroundingBorder False
      . BT.rowBorders False
      . BT.table
      . map toRow
  toRow (k, v) = [padRight (Pad 1) $ txt k, padLeft (Pad 1) $ txt v]
  glKeyBindings =
    [ ("F1", "Help")
    , ("F2", "Robots list")
    , ("F3", "Available recipes")
    , ("F4", "Available commands")
    , ("F5", "Messages")
    , ("Ctrl-g", "show goal")
    , ("Ctrl-p", "pause")
    , ("Ctrl-o", "single step")
    , ("Ctrl-z", "decrease speed")
    , ("Ctrl-w", "increase speed")
    , ("Ctrl-q", "quit the current scenario")
    , ("Meta-h", "hide robots for 2s")
    , ("Meta-w", "focus on the world map")
    , ("Meta-e", "focus on the robot inventory")
    , ("Meta-r", "focus on the REPL")
    , ("Meta-t", "focus on the info panel")
    ]

data NotificationList = RecipeList | MessageList

availableListWidget :: GameState -> NotificationList -> Widget Name
availableListWidget gs nl = padTop (Pad 1) $ vBox widgetList
 where
  widgetList = case nl of
    RecipeList -> mkAvailableList gs availableRecipes renderRecipe
    MessageList -> messagesWidget gs
  renderRecipe = padLeftRight 18 . drawRecipe Nothing (fromMaybe E.empty inv)
  inv = gs ^? to focusedRobot . _Just . robotInventory

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

commandsListWidget :: GameState -> Widget Name
commandsListWidget gs =
  hCenter $
    vBox
      [ table
      , padTop (Pad 1) $ txt "For the full list of available commands see the Wiki at:"
      , txt "https://github.com/swarm-game/swarm/wiki/Commands-Cheat-Sheet"
      ]
 where
  commands = gs ^. availableCommands . notificationsContent
  table =
    BT.renderTable
      . BT.surroundingBorder False
      . BT.columnBorders False
      . BT.rowBorders False
      . BT.setDefaultColAlignment BT.AlignLeft
      . BT.alignRight 0
      . BT.table
      $ headers : commandsTable
  headers =
    withAttr robotAttr
      <$> [ txt "command name"
          , txt " : type"
          , txt "Enabled by"
          ]

  commandsTable = mkCmdRow <$> commands
  mkCmdRow cmd =
    map
      (padTop $ Pad 1)
      [ txt $ syntax $ constInfo cmd
      , padRight (Pad 2) $ txt $ " : " <> prettyText (inferConst cmd)
      , listDevices cmd
      ]

  base = gs ^? baseRobot
  entsByCap = case base of
    Just r ->
      M.map NE.toList $
        entitiesByCapability $
          (r ^. equippedDevices) `union` (r ^. robotInventory)
    Nothing -> mempty

  listDevices cmd = vBox $ map drawLabelledEntityName providerDevices
   where
    providerDevices =
      concatMap (flip (M.findWithDefault []) entsByCap) $
        maybeToList $
          constCaps cmd

-- | Generate a pop-up widget to display the description of an entity.
descriptionWidget :: AppState -> Entity -> Widget Name
descriptionWidget s e = padLeftRight 1 (explainEntry s e)

-- | Draw a widget with messages to the current robot.
messagesWidget :: GameState -> [Widget Name]
messagesWidget gs = widgetList
 where
  widgetList = focusNewest . map drawLogEntry' $ gs ^. messageNotifications . notificationsContent
  focusNewest = if gs ^. paused then id else over _last visible
  drawLogEntry' e =
    withAttr (colorLogs e) $
      hBox
        [ fromMaybe (txt "") $ maybeDrawTime (e ^. leTime) True gs
        , padLeft (Pad 2) . txt $ "[" <> e ^. leRobotName <> "]"
        , padLeft (Pad 1) . txt2 $ e ^. leText
        ]
  txt2 = txtWrapWith indent2

colorLogs :: LogEntry -> AttrName
colorLogs e = case e ^. leSource of
  Said -> robotColor (e ^. leRobotID)
  Logged -> notifAttr
  ErrorTrace l -> case l of
    Debug -> dimAttr
    Warning -> yellowAttr
    Error -> redAttr
    Critical -> redAttr
 where
  -- color each robot message with different color of the world
  robotColor rid = fgCols !! (rid `mod` fgColLen)
  fgCols = map fst worldAttributes
  fgColLen = length fgCols

-- | Draw the F-key modal menu. This is displayed in the top left world corner.
drawModalMenu :: AppState -> Widget Name
drawModalMenu s = vLimit 1 . hBox $ map (padLeftRight 1 . drawKeyCmd) globalKeyCmds
 where
  notificationKey :: Getter GameState (Notifications a) -> Text -> Text -> Maybe (KeyHighlight, Text, Text)
  notificationKey notifLens key name
    | null (s ^. gameState . notifLens . notificationsContent) = Nothing
    | otherwise =
        let highlight
              | s ^. gameState . notifLens . notificationsCount > 0 = Alert
              | otherwise = NoHighlight
         in Just (highlight, key, name)

  globalKeyCmds =
    catMaybes
      [ Just (NoHighlight, "F1", "Help")
      , Just (NoHighlight, "F2", "Robots")
      , notificationKey availableRecipes "F3" "Recipes"
      , notificationKey availableCommands "F4" "Commands"
      , notificationKey messageNotifications "F5" "Messages"
      ]

-- | Draw a menu explaining what key commands are available for the
--   current panel.  This menu is displayed as a single line in
--   between the world panel and the REPL.
--
-- This excludes the F-key modals that are shown elsewhere.
drawKeyMenu :: AppState -> Widget Name
drawKeyMenu s =
  vLimit 2 $
    hBox
      [ vBox
          [ mkCmdRow globalKeyCmds
          , padLeft (Pad 2) $ mkCmdRow focusedPanelCmds
          ]
      , gameModeWidget
      ]
 where
  mkCmdRow = hBox . map drawPaddedCmd
  drawPaddedCmd = padLeftRight 1 . drawKeyCmd
  focusedPanelCmds =
    map highlightKeyCmds $
      keyCmdsFor $
        focusGetCurrent $
          view (uiState . uiFocusRing) s

  isReplWorking = s ^. gameState . replWorking
  isPaused = s ^. gameState . paused
  viewingBase = (s ^. gameState . viewCenterRule) == VCRobot 0
  creative = s ^. gameState . creativeMode
  cheat = s ^. uiState . uiCheatMode
  goal = hasAnythingToShow $ s ^. uiState . uiGoal . goalsContent
  showZero = s ^. uiState . uiShowZero
  inventorySort = s ^. uiState . uiInventorySort
  ctrlMode = s ^. uiState . uiREPL . replControlMode

  renderControlModeSwitch :: ReplControlMode -> T.Text
  renderControlModeSwitch = \case
    Piloting -> "REPL"
    Typing -> "pilot"

  gameModeWidget =
    padLeft Max
      . padLeftRight 1
      . txt
      . (<> " mode")
      $ case creative of
        False -> "Classic"
        True -> "Creative"
  globalKeyCmds =
    catMaybes
      [ may goal (NoHighlight, "^g", "goal")
      , may cheat (NoHighlight, "^v", "creative")
      , Just (NoHighlight, "^p", if isPaused then "unpause" else "pause")
      , Just (NoHighlight, "^o", "step")
      , Just (NoHighlight, "^zx", "speed")
      , Just (if s ^. uiState . uiShowRobots then NoHighlight else Alert, "M-h", "hide robots")
      ]
  may b = if b then Just else const Nothing

  highlightKeyCmds (k, n) = (,k,n) $ case n of
    "pop out" | (s ^. uiState . uiMoreInfoBot) || (s ^. uiState . uiMoreInfoTop) -> Alert
    _ -> PanelSpecific

  keyCmdsFor (Just (FocusablePanel REPLPanel)) =
    [ ("↓↑", "history")
    ]
      ++ [("Enter", "execute") | not isReplWorking]
      ++ [("^c", "cancel") | isReplWorking]
      ++ [("M-p", renderControlModeSwitch ctrlMode) | creative]
  keyCmdsFor (Just (FocusablePanel WorldPanel)) =
    [ ("←↓↑→ / hjkl", "scroll") | creative
    ]
      ++ [("c", "recenter") | not viewingBase]
      ++ [("f", "FPS")]
  keyCmdsFor (Just (FocusablePanel RobotPanel)) =
    [ ("Enter", "pop out")
    , ("m", "make")
    , ("0", (if showZero then "hide" else "show") <> " 0")
    , (":/;", T.unwords ["Sort:", renderSortMethod inventorySort])
    ]
  keyCmdsFor (Just (FocusablePanel InfoPanel)) = []
  keyCmdsFor _ = []

data KeyHighlight = NoHighlight | Alert | PanelSpecific

-- | Draw a single key command in the menu.
drawKeyCmd :: (KeyHighlight, Text, Text) -> Widget Name
drawKeyCmd (h, key, cmd) =
  hBox
    [ withAttr attr (txt $ T.concat ["[", key, "] "])
    , txt cmd
    ]
 where
  attr = case h of
    NoHighlight -> defAttr
    Alert -> notifAttr
    PanelSpecific -> highlightAttr

------------------------------------------------------------
-- World panel
------------------------------------------------------------

-- | Draw the current world view.
drawWorld :: Bool -> GameState -> Widget Name
drawWorld showRobots g =
  center
    . cached WorldCache
    . reportExtent WorldExtent
    -- Set the clickable request after the extent to play nice with the cache
    . clickable (FocusablePanel WorldPanel)
    . Widget Fixed Fixed
    $ do
      ctx <- getContext
      let w = ctx ^. availWidthL
          h = ctx ^. availHeightL
          ixs = range (viewingRegion g (fromIntegral w, fromIntegral h))
      render . vBox . map hBox . chunksOf w . map (drawLoc showRobots g) $ ixs

------------------------------------------------------------
-- Robot inventory panel
------------------------------------------------------------

-- | Draw info about the currently focused robot, such as its name,
--   position, orientation, and inventory.
drawRobotPanel :: AppState -> Widget Name
drawRobotPanel s = case (s ^. gameState . to focusedRobot, s ^. uiState . uiInventory) of
  (Just r, Just (_, lst)) ->
    let Location x y = r ^. robotLocation
        drawClickableItem pos selb = clickable (InventoryListItem pos) . drawItem (lst ^. BL.listSelectedL) pos selb
     in padBottom Max $
          vBox
            [ hCenter $
                hBox
                  [ txt (r ^. robotName)
                  , padLeft (Pad 2) $ str (printf "(%d, %d)" x y)
                  , padLeft (Pad 2) $ renderDisplay (r ^. robotDisplay)
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
drawItem _ _ _ (EquippedEntry e) = drawLabelledEntityName e <+> padLeft Max (str " ")

-- | Draw the name of an entity, labelled with its visual
--   representation as a cell in the world.
drawLabelledEntityName :: Entity -> Widget Name
drawLabelledEntityName e =
  hBox
    [ padRight (Pad 2) (renderDisplay (e ^. entityDisplay))
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
  Just (EquippedEntry e) ->
    explainEntry s e
      -- Special case: equipped logger device displays the robot's log.
      <=> if e ^. entityName == "logger" then drawRobotLog s else emptyWidget
  _ -> txt " "

explainEntry :: AppState -> Entity -> Widget Name
explainEntry s e =
  vBox
    [ displayProperties $ Set.toList (e ^. entityProperties)
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

-- | Return all recipes that involve a given entity.
recipesWith :: AppState -> Entity -> [Recipe Entity]
recipesWith s e =
  let getRecipes select = recipesFor (s ^. gameState . select) e
   in -- The order here is chosen intentionally.  See https://github.com/swarm-game/swarm/issues/418.
      --
      --   1. Recipes where the entity is an input --- these should go
      --     first since the first thing you will want to know when you
      --     obtain a new entity is what you can do with it.
      --
      --   2. Recipes where it serves as a catalyst --- for the same reason.
      --
      --   3. Recipes where it is an output --- these should go last,
      --      since if you have it, you probably already figured out how
      --      to make it.
      L.nub $ getRecipes recipesIn ++ getRecipes recipesReq ++ getRecipes recipesOut

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
    | Just ingr == me = withAttr highlightAttr $ txtLines nm
    | ingr == timeE = withAttr yellowAttr $ txtLines nm
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
    , vBox . F.toList . imap drawEntry $ logEntries
    ]
 where
  logEntries = s ^. gameState . to focusedRobot . _Just . robotLog

  rn = s ^? gameState . to focusedRobot . _Just . robotName
  n = Seq.length logEntries

  allMe = all ((== rn) . Just . view leRobotName) logEntries

  drawEntry i e =
    (if i == n - 1 && s ^. uiState . uiScrollToEnd then visible else id) $
      drawLogEntry (not allMe) e

-- | Draw one log entry with an optional robot name first.
drawLogEntry :: Bool -> LogEntry -> Widget a
drawLogEntry addName e = withAttr (colorLogs e) . txtWrapWith indent2 $ if addName then name else t
 where
  t = e ^. leText
  name = "[" <> view leRobotName e <> "] " <> (if e ^. leSource == Said then "said " <> quote t else t)

------------------------------------------------------------
-- REPL panel
------------------------------------------------------------

-- | Turn the repl prompt into a decorator for the form
replPromptAsWidget :: Text -> REPLPrompt -> Widget Name
replPromptAsWidget _ (CmdPrompt _) = txt "> "
replPromptAsWidget t (SearchPrompt rh) =
  case lastEntry t rh of
    Nothing -> txt "[nothing found] "
    Just lastentry
      | T.null t -> txt "[find] "
      | otherwise -> txt $ "[found: \"" <> lastentry <> "\"] "

renderREPLPrompt :: FocusRing Name -> REPLState -> Widget Name
renderREPLPrompt focus repl = ps1 <+> replE
 where
  prompt = repl ^. replPromptType
  replEditor = repl ^. replPromptEditor
  color = if repl ^. replValid then id else withAttr redAttr
  ps1 = replPromptAsWidget (T.concat $ getEditContents replEditor) prompt
  replE =
    renderEditor
      (color . vBox . map txt)
      (focusGetCurrent focus `elem` [Nothing, Just (FocusablePanel REPLPanel), Just REPLInput])
      replEditor

-- | Draw the REPL.
drawREPL :: AppState -> Widget Name
drawREPL s = vBox $ latestHistory <> [currentPrompt]
 where
  -- rendered history lines fitting above REPL prompt
  latestHistory :: [Widget n]
  latestHistory = map fmt (getLatestREPLHistoryItems (replHeight - inputLines) (repl ^. replHistory))
  currentPrompt :: Widget Name
  currentPrompt = case isActive <$> base of
    Just False -> renderREPLPrompt (s ^. uiState . uiFocusRing) repl
    _running -> padRight Max $ txt "..."
  inputLines = 1
  repl = s ^. uiState . uiREPL
  base = s ^. gameState . robotMap . at 0
  fmt (REPLEntry e) = txt $ "> " <> e
  fmt (REPLOutput t) = txt t
