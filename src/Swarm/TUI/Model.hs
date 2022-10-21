{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Swarm.TUI.Model
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Application state for the @brick@-based Swarm TUI.
module Swarm.TUI.Model (
  -- * Custom UI label types
  -- $uilabel
  AppEvent (..),
  Name (..),

  -- * Menus and dialogs
  ModalType (..),
  ButtonSelection (..),
  Modal (..),
  modalType,
  modalDialog,
  MainMenuEntry (..),
  mainMenu,
  Menu (..),
  _NewGameMenu,
  mkScenarioList,
  mkNewGameMenu,

  -- * UI state

  -- ** REPL
  REPLHistItem (..),
  replItemText,
  isREPLEntry,
  getREPLEntry,
  REPLHistory,
  replIndex,
  replLength,
  replSeq,
  newREPLHistory,
  addREPLItem,
  restartREPLHistory,
  getLatestREPLHistoryItems,
  moveReplHistIndex,
  getCurrentItemText,
  replIndexIsAtInput,
  TimeDir (..),

  -- ** Prompt utils
  REPLPrompt (..),
  mkCmdPrompt,
  replPromptAsWidget,
  promptTextL,
  promptUpdateL,
  mkReplForm,
  removeEntry,
  resetWithREPLForm,

  -- ** Inventory
  InventoryListEntry (..),
  _Separator,
  _InventoryEntry,
  _InstalledEntry,

  -- ** UI Model
  UIState,
  uiMenu,
  uiPlaying,
  uiCheatMode,
  uiFocusRing,
  uiWorldCursor,
  uiReplForm,
  uiReplType,
  uiReplHistory,
  uiReplLast,
  uiInventory,
  uiMoreInfoTop,
  uiMoreInfoBot,
  uiScrollToEnd,
  uiError,
  uiModal,
  uiGoal,
  lgTicksPerSecond,
  lastFrameTime,
  accumulatedTime,
  tickCount,
  frameCount,
  frameTickCount,
  lastInfoTime,
  uiShowFPS,
  uiShowZero,
  uiInventoryShouldUpdate,
  uiTPF,
  uiFPS,
  scenarioRef,
  appData,

  -- ** Initialization
  initFocusRing,
  defaultPrompt,
  initReplForm,
  initLgTicksPerSecond,
  initUIState,
  lastEntry,

  -- ** Updating
  populateInventoryList,
  infoScroll,
  modalScroll,

  -- * Runtime state
  RuntimeState,
  webPort,
  upstreamRelease,
  eventLog,
  logEvent,

  -- * App state
  AppState,
  gameState,
  uiState,
  runtimeState,

  -- ** Initialization
  AppOpts (..),
  initAppState,
  startGame,
  restartGame,
  scenarioToAppState,
  Seed,

  -- ** Utility
  focusedItem,
  focusedEntity,
  nextScenario,
  initRuntimeState,
) where

import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Dialog (Dialog)
import Brick.Widgets.List qualified as BL
import Control.Applicative (Applicative (liftA2))
import Control.Lens hiding (from, (<.>))
import Control.Monad.Except
import Control.Monad.State
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Foldable (toList)
import Data.List (findIndex, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Data.Vector qualified as V
import GitHash (GitInfo)
import Linear (zero)
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.Entity as E
import Swarm.Game.Robot
import Swarm.Game.Scenario (loadScenario)
import Swarm.Game.ScenarioInfo (
  ScenarioCollection,
  ScenarioInfo (..),
  ScenarioInfoPair,
  ScenarioItem (..),
  ScenarioStatus (..),
  normalizeScenarioPath,
  scMap,
  scenarioCollectionToList,
  scenarioItemByPath,
  scenarioPath,
  scenarioStatus,
  _SISingle,
 )
import Swarm.Game.State
import Swarm.Game.World qualified as W
import Swarm.Language.Types
import Swarm.Util
import Swarm.Version (NewReleaseFailure (NoMainUpstreamRelease))
import System.Clock
import System.FilePath (dropTrailingPathSeparator, splitPath, takeFileName)
import Witch (into)

------------------------------------------------------------
-- Custom UI label types
------------------------------------------------------------

-- $uilabel These types are used as parameters to various @brick@
-- types.

-- | 'Swarm.TUI.Model.AppEvent' represents a type for custom event types our app can
--   receive.  At the moment, we only have one custom event, but it's
--   very important: a separate thread sends 'Frame' events as fast as
--   it can, telling the TUI to render a new frame.
data AppEvent
  = Frame
  | UpstreamVersion (Either NewReleaseFailure String)
  deriving (Show)

-- | 'Name' represents names to uniquely identify various components
--   of the UI, such as forms, panels, caches, extents, and lists.
data Name
  = -- | The panel containing the REPL.
    REPLPanel
  | -- | The panel containing the world view.
    WorldPanel
  | -- | The panel showing robot info and inventory on the top left.
    RobotPanel
  | -- | The info panel on the bottom left.
    InfoPanel
  | -- | The REPL input form.
    REPLInput
  | -- | The render cache for the world view.
    WorldCache
  | -- | The cached extent for the world view.
    WorldExtent
  | -- | The list of inventory items for the currently
    --   focused robot.
    InventoryList
  | -- | The inventory item position in the InventoryList.
    InventoryListItem Int
  | -- | The list of main menu choices.
    MenuList
  | -- | The list of scenario choices.
    ScenarioList
  | -- | The scrollable viewport for the info panel.
    InfoViewport
  | -- | The scrollable viewport for any modal dialog.
    ModalViewport
  deriving (Eq, Ord, Show, Read)

infoScroll :: ViewportScroll Name
infoScroll = viewportScroll InfoViewport

modalScroll :: ViewportScroll Name
modalScroll = viewportScroll ModalViewport

------------------------------------------------------------
-- REPL History
------------------------------------------------------------

-- | An item in the REPL history.
data REPLHistItem
  = -- | Something entered by the user.
    REPLEntry Text
  | -- | A response printed by the system.
    REPLOutput Text
  deriving (Eq, Ord, Show, Read)

-- | Useful helper function to only get user input text.
getREPLEntry :: REPLHistItem -> Maybe Text
getREPLEntry = \case
  REPLEntry t -> Just t
  _ -> Nothing

-- | Useful helper function to filter out REPL output.
isREPLEntry :: REPLHistItem -> Bool
isREPLEntry = isJust . getREPLEntry

-- | Get the text of REPL input/output.
replItemText :: REPLHistItem -> Text
replItemText = \case
  REPLEntry t -> t
  REPLOutput t -> t

-- | History of the REPL with indices (0 is first entry) to the current
--   line and to the first entry since loading saved history.
--   We also (ab)use the length of the REPL as the index of current
--   input line, since that number is one past the index of last entry.
data REPLHistory = REPLHistory
  { _replSeq :: Seq REPLHistItem
  , _replIndex :: Int
  , _replStart :: Int
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''REPLHistory

-- | Sequence of REPL inputs and outputs, oldest entry is leftmost.
replSeq :: Lens' REPLHistory (Seq REPLHistItem)

-- | The current index in the REPL history (if the user is going back
--   through the history using up/down keys).
replIndex :: Lens' REPLHistory Int

-- | The index of the first entry since loading saved history.
--
-- It will be set on load and reset on save (happens during exit).
replStart :: Lens' REPLHistory Int

-- | Create new REPL history (i.e. from loaded history file lines).
newREPLHistory :: [REPLHistItem] -> REPLHistory
newREPLHistory xs =
  let s = Seq.fromList xs
   in REPLHistory
        { _replSeq = s
        , _replStart = length s
        , _replIndex = length s
        }

-- | Point the start of REPL history after current last line. See 'replStart'.
restartREPLHistory :: REPLHistory -> REPLHistory
restartREPLHistory h = h & replStart .~ replLength h

-- | Current number lines of the REPL history - (ab)used as index of input buffer.
replLength :: REPLHistory -> Int
replLength = length . _replSeq

-- | Add new REPL input - the index must have been pointing one past
--   the last element already, so we increment it to keep it that way.
addREPLItem :: REPLHistItem -> REPLHistory -> REPLHistory
addREPLItem t h =
  h
    & replSeq %~ (|> t)
    & replIndex .~ 1 + replLength h

-- | Get the latest N items in history, starting with the oldest one.
--
-- This is used to show previous REPL lines in UI, so we need the items
-- sorted in the order they were entered and will be drawn top to bottom.
getLatestREPLHistoryItems :: Int -> REPLHistory -> [REPLHistItem]
getLatestREPLHistoryItems n h = toList latestN
 where
  latestN = Seq.drop oldestIndex $ h ^. replSeq
  oldestIndex = max (h ^. replStart) $ length (h ^. replSeq) - n

data TimeDir = Newer | Older deriving (Eq, Ord, Show)

moveReplHistIndex :: TimeDir -> Text -> REPLHistory -> REPLHistory
moveReplHistIndex d lastEntered history = history & replIndex .~ newIndex
 where
  historyLen = replLength history
  curText = fromMaybe lastEntered $ getCurrentItemText history
  curIndex = history ^. replIndex
  entries = history ^. replSeq
  -- split repl at index
  (olderP, newer) = Seq.splitAt curIndex entries
  -- find first different entry in direction
  notSameEntry = \case
    REPLEntry t -> t /= curText
    _ -> False
  newIndex = case d of
    Newer -> maybe historyLen (curIndex +) $ Seq.findIndexL notSameEntry newer
    Older -> fromMaybe curIndex $ Seq.findIndexR notSameEntry olderP

getCurrentItemText :: REPLHistory -> Maybe Text
getCurrentItemText history = replItemText <$> Seq.lookup (history ^. replIndex) (history ^. replSeq)

replIndexIsAtInput :: REPLHistory -> Bool
replIndexIsAtInput repl = repl ^. replIndex == replLength repl

------------------------------------------------------------
-- Repl Prompt
------------------------------------------------------------

-- | This data type represent what is prompted to the player
--   and how the REPL show interpret the user input.
data REPLPrompt
  = -- | Interpret the given text as a regular command.
    --   The list is for potential completions, which we can
    --   cycle through by hitting Tab repeatedly
    CmdPrompt Text [Text]
  | -- | Interpret the given text as "search this text in history"
    SearchPrompt Text REPLHistory

-- | Get the last REPLEntry in REPLHistory matching the given text
lastEntry :: Text -> REPLHistory -> Maybe Text
lastEntry t h =
  case Seq.viewr $ Seq.filter matchEntry $ h ^. replSeq of
    Seq.EmptyR -> Nothing
    _ Seq.:> a -> Just (replItemText a)
 where
  matchesText histItem = t `T.isInfixOf` replItemText histItem
  matchEntry = liftA2 (&&) matchesText isREPLEntry

-- | Given some text,  removes the REPLEntry within REPLHistory which is equal to that.
--   This is used when the user enters in search mode and want to traverse the history.
--   If a command has been used many times, the history will be populated with it causing
--   the effect that search command always finds the same command.
removeEntry :: Text -> REPLHistory -> REPLHistory
removeEntry foundtext hist = hist & replSeq %~ Seq.filter (/= REPLEntry foundtext)

mkCmdPrompt :: Text -> REPLPrompt
mkCmdPrompt t = CmdPrompt t []

defaultPrompt :: REPLPrompt
defaultPrompt = mkCmdPrompt ""

-- | Lens for accesing the text of the prompt.
--   Notice that setting the text clears any pending completions.
promptTextL :: Lens' REPLPrompt Text
promptTextL = lens g s
 where
  -- Notice that the prompt ADT must have a Text field in every constructor (representing what the user writes).
  -- This should be force in the ADT itself... right know this here
  -- The compiler will complain about "Non complete patterns" on this two function.
  g :: REPLPrompt -> Text
  g (CmdPrompt t _) = t
  g (SearchPrompt t _) = t

  s :: REPLPrompt -> Text -> REPLPrompt
  s (CmdPrompt _ _) t = mkCmdPrompt t
  s (SearchPrompt _ h) t = SearchPrompt t h

-- | Turn the repl prompt into a decorator for the form
replPromptAsWidget :: REPLPrompt -> Widget Name
replPromptAsWidget (CmdPrompt {}) = txt "> "
replPromptAsWidget (SearchPrompt t rh) =
  case lastEntry t rh of
    Nothing -> txt "[nothing found] "
    Just lastentry
      | T.null t -> txt "[find] "
      | otherwise -> txt $ "[found: \"" <> lastentry <> "\"] "

-- | Creates the repl form as a decorated form.
mkReplForm :: REPLPrompt -> Form REPLPrompt AppEvent Name
mkReplForm r = newForm [(replPromptAsWidget r <+>) @@= editTextField promptTextL REPLInput (Just 1)] r

------------------------------------------------------------
-- Menus and dialogs
------------------------------------------------------------

data ModalType
  = HelpModal
  | RecipesModal
  | CommandsModal
  | MessagesModal
  | RobotsModal
  | WinModal
  | QuitModal
  | KeepPlayingModal
  | DescriptionModal Entity
  | GoalModal [Text]
  deriving (Eq, Show)

data ButtonSelection = CancelButton | KeepPlayingButton | StartOverButton ScenarioInfoPair | QuitButton | NextButton ScenarioInfoPair

data Modal = Modal
  { _modalType :: ModalType
  , _modalDialog :: Dialog ButtonSelection
  }

makeLenses ''Modal

data MainMenuEntry = NewGame | Tutorial | Messages | About | Quit
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Menu
  = NoMenu -- We started playing directly from command line, no menu to show
  | MainMenu (BL.List Name MainMenuEntry)
  | NewGameMenu (NonEmpty (BL.List Name ScenarioItem)) -- stack of scenario item lists
  | MessagesMenu
  | AboutMenu

mainMenu :: MainMenuEntry -> BL.List Name MainMenuEntry
mainMenu e = BL.list MenuList (V.fromList [minBound .. maxBound]) 1 & BL.listMoveToElement e

makePrisms ''Menu

-- | Create a brick 'BL.List' of scenario items from a 'ScenarioCollection'.
mkScenarioList :: Bool -> ScenarioCollection -> BL.List Name ScenarioItem
mkScenarioList cheat = flip (BL.list ScenarioList) 1 . V.fromList . filterTest . scenarioCollectionToList
 where
  filterTest = if cheat then id else filter (\case SICollection n _ -> n /= "Testing"; _ -> True)

-- | Given a 'ScenarioCollection' and a 'FilePath' which is the canonical
--   path to some folder or scenario, construct a 'NewGameMenu' stack
--   focused on the given item, if possible.
mkNewGameMenu :: Bool -> ScenarioCollection -> FilePath -> Maybe Menu
mkNewGameMenu cheat sc path = NewGameMenu . NE.fromList <$> go (Just sc) (splitPath path) []
 where
  go :: Maybe ScenarioCollection -> [FilePath] -> [BL.List Name ScenarioItem] -> Maybe [BL.List Name ScenarioItem]
  go _ [] stk = Just stk
  go Nothing _ _ = Nothing
  go (Just curSC) (thing : rest) stk = go nextSC rest (lst : stk)
   where
    hasName :: ScenarioItem -> Bool
    hasName (SISingle (_, ScenarioInfo pth _ _ _)) = takeFileName pth == thing
    hasName (SICollection nm _) = nm == into @Text (dropTrailingPathSeparator thing)

    lst = BL.listFindBy hasName (mkScenarioList cheat curSC)

    nextSC = case M.lookup (dropTrailingPathSeparator thing) (scMap curSC) of
      Just (SICollection _ c) -> Just c
      _ -> Nothing

------------------------------------------------------------
-- Inventory list entries
------------------------------------------------------------

-- | An entry in the inventory list displayed in the info panel.  We
--   can either have an entity with a count in the robot's inventory,
--   an entity installed on the robot, or a labelled separator.  The
--   purpose of the separators is to show a clear distinction between
--   the robot's /inventory/ and its /installed devices/.
data InventoryListEntry
  = Separator Text
  | InventoryEntry Count Entity
  | InstalledEntry Entity
  deriving (Eq)

makePrisms ''InventoryListEntry

------------------------------------------------------------
-- UI state
------------------------------------------------------------

-- | The main record holding the UI state.  For access to the fields,
-- see the lenses below.
data UIState = UIState
  { _uiMenu :: Menu
  , _uiPlaying :: Bool
  , _uiCheatMode :: Bool
  , _uiFocusRing :: FocusRing Name
  , _uiWorldCursor :: Maybe W.Coords
  , _uiReplForm :: Form REPLPrompt AppEvent Name
  , _uiReplType :: Maybe Polytype
  , _uiReplLast :: Text
  , _uiReplHistory :: REPLHistory
  , _uiInventory :: Maybe (Int, BL.List Name InventoryListEntry)
  , _uiMoreInfoTop :: Bool
  , _uiMoreInfoBot :: Bool
  , _uiScrollToEnd :: Bool
  , _uiError :: Maybe Text
  , _uiModal :: Maybe Modal
  , _uiGoal :: Maybe [Text]
  , _uiShowFPS :: Bool
  , _uiShowZero :: Bool
  , _uiInventoryShouldUpdate :: Bool
  , _uiTPF :: Double
  , _uiFPS :: Double
  , _lgTicksPerSecond :: Int
  , _tickCount :: Int
  , _frameCount :: Int
  , _frameTickCount :: Int
  , _lastFrameTime :: TimeSpec
  , _accumulatedTime :: TimeSpec
  , _lastInfoTime :: TimeSpec
  , _appData :: Map Text Text
  , _scenarioRef :: Maybe ScenarioInfoPair
  }

--------------------------------------------------
-- Lenses for UIState

let exclude = ['_lgTicksPerSecond]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''UIState

-- | The current menu state.
uiMenu :: Lens' UIState Menu

-- | Are we currently playing the game?  True = we are playing, and
--   should thus display a world, REPL, etc.; False = we should
--   display the current menu.
uiPlaying :: Lens' UIState Bool

-- | Cheat mode, i.e. are we allowed to turn creative mode on and off?
uiCheatMode :: Lens' UIState Bool

-- | The focus ring is the set of UI panels we can cycle among using
--   the Tab key.
uiFocusRing :: Lens' UIState (FocusRing Name)

-- | The last clicked position on the world view.
uiWorldCursor :: Lens' UIState (Maybe W.Coords)

-- | The form where the user can type input at the REPL.
uiReplForm :: Lens' UIState (Form REPLPrompt AppEvent Name)

-- | The type of the current REPL input which should be displayed to
--   the user (if any).
uiReplType :: Lens' UIState (Maybe Polytype)

-- | The last thing the user has typed which isn't part of the history.
--   This is used to restore the repl form after the user visited the history.
uiReplLast :: Lens' UIState Text

-- | History of things the user has typed at the REPL, interleaved
--   with outputs the system has generated.
uiReplHistory :: Lens' UIState REPLHistory

-- | The hash value of the focused robot entity (so we can tell if its
--   inventory changed) along with a list of the items in the
--   focused robot's inventory.
uiInventory :: Lens' UIState (Maybe (Int, BL.List Name InventoryListEntry))

-- | Does the info panel contain more content past the top of the panel?
uiMoreInfoTop :: Lens' UIState Bool

-- | Does the info panel contain more content past the bottom of the panel?
uiMoreInfoBot :: Lens' UIState Bool

-- | A flag telling the UI to scroll the info panel to the very end
--   (used when a new log message is appended).
uiScrollToEnd :: Lens' UIState Bool

-- | When this is @Just@, it represents a popup box containing an
--   error message that is shown on top of the rest of the UI.
uiError :: Lens' UIState (Maybe Text)

-- | When this is @Just@, it represents a modal to be displayed on
--   top of the UI, e.g. for the Help screen.
uiModal :: Lens' UIState (Maybe Modal)

-- | Status of the scenario goal: whether there is one, and whether it
--   has been displayed to the user initially.
uiGoal :: Lens' UIState (Maybe [Text])

-- | A toggle to show the FPS by pressing `f`
uiShowFPS :: Lens' UIState Bool

-- | A toggle to show or hide inventory items with count 0 by pressing `0`
uiShowZero :: Lens' UIState Bool

-- | Whether the Inventory ui panel should update
uiInventoryShouldUpdate :: Lens' UIState Bool

-- | Computed ticks per milli seconds
uiTPF :: Lens' UIState Double

-- | Computed frames per milli seconds
uiFPS :: Lens' UIState Double

-- | The currently active Scenario description, useful for starting over.
scenarioRef :: Lens' UIState (Maybe ScenarioInfoPair)

-- | The base-2 logarithm of the current game speed in ticks/second.
--   Note that we cap this value to the range of +/- log2 INTMAX.
lgTicksPerSecond :: Lens' UIState Int
lgTicksPerSecond = lens _lgTicksPerSecond safeSetLgTicks
 where
  maxLog = finiteBitSize (maxBound :: Int)
  maxTicks = maxLog - 2
  minTicks = 2 - maxLog
  safeSetLgTicks ui lTicks
    | lTicks < minTicks = setLgTicks ui minTicks
    | lTicks > maxTicks = setLgTicks ui maxTicks
    | otherwise = setLgTicks ui lTicks
  setLgTicks ui lTicks = ui {_lgTicksPerSecond = lTicks}

-- | A counter used to track how many ticks have happened since the
--   last time we updated the ticks/frame statistics.
tickCount :: Lens' UIState Int

-- | A counter used to track how many frames have been rendered since the
--   last time we updated the ticks/frame statistics.
frameCount :: Lens' UIState Int

-- | A counter used to track how many ticks have happened in the
--   current frame, so we can stop when we get to the tick cap.
frameTickCount :: Lens' UIState Int

-- | The time of the last info widget update
lastInfoTime :: Lens' UIState TimeSpec

-- | The time of the last 'Frame' event.
lastFrameTime :: Lens' UIState TimeSpec

-- | The amount of accumulated real time.  Every time we get a 'Frame'
--   event, we accumulate the amount of real time that happened since
--   the last frame, then attempt to take an appropriate number of
--   ticks to "catch up", based on the target tick rate.
--
--   See https://gafferongames.com/post/fix_your_timestep/ .
accumulatedTime :: Lens' UIState TimeSpec

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' UIState (Map Text Text)

-- | Lens for accesing the text of the prompt.
--   Notice that setting the text clears any pending completions.
promptUpdateL :: Lens UIState (Form REPLPrompt AppEvent Name) Text Text
promptUpdateL = lens g s
 where
  -- Notice that the prompt ADT must have a Text field in every constructor (representing what the user writes).
  -- This should be force in the ADT itself... right know this here
  -- The compiler will complain about "Non complete patterns" on this two function.
  g :: UIState -> Text
  g ui = case formState (ui ^. uiReplForm) of
    CmdPrompt t _ -> t
    SearchPrompt t _ -> t

  s :: UIState -> Text -> Form REPLPrompt AppEvent Name
  s ui inputText = case formState (ui ^. uiReplForm) of
    CmdPrompt _ _ -> mkReplForm $ mkCmdPrompt inputText
    SearchPrompt _ _ -> mkReplForm $ SearchPrompt inputText (ui ^. uiReplHistory)

-- ----------------------------------------------------------------------------
--                                Runtime state                              --
-- ----------------------------------------------------------------------------

data RuntimeState = RuntimeState
  { _webPort :: Maybe Port
  , _upstreamRelease :: Either NewReleaseFailure String
  , _eventLog :: Notifications LogEntry
  }

initRuntimeState :: RuntimeState
initRuntimeState =
  RuntimeState
    { _webPort = Nothing
    , _upstreamRelease = Left (NoMainUpstreamRelease [])
    , _eventLog = mempty
    }

makeLensesWith (lensRules & generateSignatures .~ False) ''RuntimeState

-- | The port on which the HTTP debug service is running.
webPort :: Lens' RuntimeState (Maybe Port)

-- | The upstream release version.
upstreamRelease :: Lens' RuntimeState (Either NewReleaseFailure String)

-- | A log of runtime events.
--
-- This logging is separate from the logging done during game-play.
-- If some error happens before a game is even selected, this is the
-- place to log it.
eventLog :: Lens' RuntimeState (Notifications LogEntry)

-- | Simply log to the runtime event log.
logEvent :: LogSource -> (Text, RID) -> Text -> Notifications LogEntry -> Notifications LogEntry
logEvent src (who, rid) msg el =
  el
    & notificationsCount %~ succ
    & notificationsContent %~ (l :)
 where
  l = LogEntry 0 src who rid zero msg

-- ----------------------------------------------------------------------------
--                                   APPSTATE                                --
-- ----------------------------------------------------------------------------

-- | The 'AppState' just stores together the other states.
--
-- This is so you can use a smaller state when e.g. writing some game logic
-- or updating the UI. Also consider that GameState can change when loading
-- a new scenario - if the state should persist games, use RuntimeState.
data AppState = AppState
  { _gameState :: GameState
  , _uiState :: UIState
  , _runtimeState :: RuntimeState
  }

--------------------------------------------------
-- Lenses for AppState

makeLensesWith (lensRules & generateSignatures .~ False) ''AppState

-- | The 'GameState' record.
gameState :: Lens' AppState GameState

-- | The 'UIState' record.
uiState :: Lens' AppState UIState

-- | The 'RuntimeState' record
runtimeState :: Lens' AppState RuntimeState

--------------------------------------------------
-- Utility functions

-- | Get the currently focused 'InventoryListEntry' from the robot
--   info panel (if any).
focusedItem :: AppState -> Maybe InventoryListEntry
focusedItem s = do
  list <- s ^? uiState . uiInventory . _Just . _2
  (_, entry) <- BL.listSelectedElement list
  return entry

-- | Get the currently focused entity from the robot info panel (if
--   any).  This is just like 'focusedItem' but forgets the
--   distinction between plain inventory items and installed devices.
focusedEntity :: AppState -> Maybe Entity
focusedEntity =
  focusedItem >=> \case
    Separator _ -> Nothing
    InventoryEntry _ e -> Just e
    InstalledEntry e -> Just e

--------------------------------------------------
-- UIState initialization

-- | The initial state of the focus ring.
initFocusRing :: FocusRing Name
initFocusRing = focusRing [REPLPanel, InfoPanel, RobotPanel, WorldPanel]

-- | The initial state of the REPL entry form.
initReplForm :: Form REPLPrompt AppEvent Name
initReplForm =
  newForm
    [(replPromptAsWidget defaultPrompt <+>) @@= editTextField promptTextL REPLInput (Just 1)]
    (mkCmdPrompt "")

-- | The initial tick speed.
initLgTicksPerSecond :: Int
initLgTicksPerSecond = 4 -- 2^4 = 16 ticks / second

-- | Initialize the UI state.  This needs to be in the IO monad since
--   it involves reading a REPL history file, getting the current
--   time, and loading text files from the data directory.  The @Bool@
--   parameter indicates whether we should start off by showing the
--   main menu.
initUIState :: Bool -> Bool -> ExceptT Text IO UIState
initUIState showMainMenu cheatMode = liftIO $ do
  historyT <- readFileMayT =<< getSwarmHistoryPath False
  appDataMap <- readAppData
  let history = maybe [] (map REPLEntry . T.lines) historyT
  startTime <- getTime Monotonic
  return $
    UIState
      { _uiMenu = if showMainMenu then MainMenu (mainMenu NewGame) else NoMenu
      , _uiPlaying = not showMainMenu
      , _uiCheatMode = cheatMode
      , _uiFocusRing = initFocusRing
      , _uiWorldCursor = Nothing
      , _uiReplForm = initReplForm
      , _uiReplType = Nothing
      , _uiReplHistory = newREPLHistory history
      , _uiReplLast = ""
      , _uiInventory = Nothing
      , _uiMoreInfoTop = False
      , _uiMoreInfoBot = False
      , _uiScrollToEnd = False
      , _uiError = Nothing
      , _uiModal = Nothing
      , _uiGoal = Nothing
      , _uiShowFPS = False
      , _uiShowZero = True
      , _uiInventoryShouldUpdate = False
      , _uiTPF = 0
      , _uiFPS = 0
      , _lgTicksPerSecond = initLgTicksPerSecond
      , _lastFrameTime = startTime
      , _accumulatedTime = 0
      , _lastInfoTime = 0
      , _tickCount = 0
      , _frameCount = 0
      , _frameTickCount = 0
      , _appData = appDataMap
      , _scenarioRef = Nothing
      }

------------------------------------------------------------
-- Functions for updating the UI state
------------------------------------------------------------

-- | Given the focused robot, populate the UI inventory list in the info
--   panel with information about its inventory.
populateInventoryList :: MonadState UIState m => Maybe Robot -> m ()
populateInventoryList Nothing = uiInventory .= Nothing
populateInventoryList (Just r) = do
  mList <- preuse (uiInventory . _Just . _2)
  showZero <- use uiShowZero
  let mkInvEntry (n, e) = InventoryEntry n e
      mkInstEntry (_, e) = InstalledEntry e
      itemList mk label =
        (\case [] -> []; xs -> Separator label : xs)
          . map mk
          . sortOn (view entityName . snd)
          . filter shouldDisplay
          . elems

      -- Display items if we have a positive number of them, or they
      -- aren't an installed device.  In other words we don't need to
      -- display installed devices twice unless we actually have some
      -- in our inventory in addition to being installed.
      shouldDisplay (n, e) = n > 0 || showZero && not ((r ^. installedDevices) `E.contains` e)

      items =
        (r ^. robotInventory . to (itemList mkInvEntry "Inventory"))
          ++ (r ^. installedDevices . to (itemList mkInstEntry "Installed devices"))

      -- Attempt to keep the selected element steady.
      sel = mList >>= BL.listSelectedElement -- Get the currently selected element+index.
      idx = case sel of
        -- If there is no currently selected element, just focus on
        -- index 1 (not 0, to avoid the separator).
        Nothing -> 1
        -- Otherwise, try to find the same entry in the list;
        -- if it's not there, keep the index the same.
        Just (selIdx, InventoryEntry _ e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview (_InventoryEntry . _2)) items)
        Just (selIdx, InstalledEntry e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview _InstalledEntry) items)
        Just (selIdx, _) -> selIdx

      -- Create the new list, focused at the desired index.
      lst = BL.listMoveTo idx $ BL.list InventoryList (V.fromList items) 1

  -- Finally, populate the newly created list in the UI, and remember
  -- the hash of the current robot.
  uiInventory .= Just (r ^. inventoryHash, lst)

-- | Set the REPLForm to the given value, resetting type error checks to Nothing
--   and removing uiError.
resetWithREPLForm :: Form REPLPrompt AppEvent Name -> UIState -> UIState
resetWithREPLForm f =
  (uiReplForm .~ f)
    . (uiError .~ Nothing)

------------------------------------------------------------
-- App state (= UI state + game state) initialization
------------------------------------------------------------

-- | Command-line options for configuring the app.
data AppOpts = AppOpts
  { -- | Explicit seed chosen by the user.
    userSeed :: Maybe Seed
  , -- | Scenario the user wants to play.
    userScenario :: Maybe FilePath
  , -- | Code to be run on base.
    toRun :: Maybe FilePath
  , -- | Should cheat mode be enabled?
    cheatMode :: Bool
  , -- | Explicit port on which to run the web API
    userWebPort :: Maybe Port
  , -- | Information about the Git repository (not present in release).
    repoGitInfo :: Maybe GitInfo
  }

-- | Initialize the 'AppState'.
initAppState :: AppOpts -> ExceptT Text IO AppState
initAppState AppOpts {..} = do
  let skipMenu = isJust userScenario || isJust toRun || isJust userSeed
  gs <- initGameState
  ui <- initUIState (not skipMenu) cheatMode
  let rs = initRuntimeState
  case skipMenu of
    False -> return $ AppState gs ui rs
    True -> do
      (scenario, path) <- loadScenario (fromMaybe "classic" userScenario) (gs ^. entityMap)
      execStateT
        (startGameWithSeed userSeed (scenario, ScenarioInfo path NotStarted NotStarted NotStarted) toRun)
        (AppState gs ui rs)

-- | Load a 'Scenario' and start playing the game.
startGame :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> Maybe FilePath -> m ()
startGame = startGameWithSeed Nothing

-- | Re-initialize the game from the stored reference to the current scenario.
--
-- Note that "restarting" is intended only for "scenarios";
-- with some scenarios, it may be possible to get stuck so that it is
-- either impossible or very annoying to win, so being offered an
-- option to restart is more user-friendly.
--
-- Since scenarios are stored as a Maybe in the UI state, we handle the Nothing
-- case upstream so that the Scenario passed to this function definitely exists.
restartGame :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> m ()
restartGame siPair = startGame siPair Nothing

-- | Load a 'Scenario' and start playing the game, with the
--   possibility for the user to override the seed.
startGameWithSeed :: (MonadIO m, MonadState AppState m) => Maybe Seed -> ScenarioInfoPair -> Maybe FilePath -> m ()
startGameWithSeed userSeed siPair@(_scene, si) toRun = do
  t <- liftIO getZonedTime
  ss <- use $ gameState . scenarios
  p <- liftIO $ normalizeScenarioPath ss (si ^. scenarioPath)
  gameState . currentScenarioPath .= Just p
  gameState . scenarios . scenarioItemByPath p . _SISingle . _2 . scenarioStatus .= InProgress t 0 0
  scenarioToAppState siPair userSeed toRun

-- | Extract the scenario which would come next in the menu from the
--   currently selected scenario (if any).  Can return @Nothing@ if
--   either we are not in the @NewGameMenu@, or the current scenario
--   is the last among its siblings.
nextScenario :: Menu -> Maybe ScenarioInfoPair
nextScenario = \case
  NewGameMenu (curMenu :| _) ->
    let nextMenuList = BL.listMoveDown curMenu
        isLastScenario = BL.listSelected curMenu == Just (length (BL.listElements curMenu) - 1)
     in if isLastScenario
          then Nothing
          else BL.listSelectedElement nextMenuList >>= preview _SISingle . snd
  _ -> Nothing

-- XXX do we need to keep an old entity map around???

-- | Modify the 'AppState' appropriately when starting a new scenario.
scenarioToAppState :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> Maybe Seed -> Maybe String -> m ()
scenarioToAppState siPair@(scene, _) userSeed toRun = do
  withLensIO gameState $ scenarioToGameState scene userSeed toRun
  withLensIO uiState $ scenarioToUIState siPair
 where
  withLensIO :: (MonadIO m, MonadState AppState m) => Lens' AppState x -> (x -> IO x) -> m ()
  withLensIO l a = do
    x <- use l
    x' <- liftIO $ a x
    l .= x'

-- | Modify the UI state appropriately when starting a new scenario.
scenarioToUIState :: ScenarioInfoPair -> UIState -> IO UIState
scenarioToUIState siPair u =
  return $
    u
      & uiPlaying .~ True
      & uiGoal .~ Nothing
      & uiFocusRing .~ initFocusRing
      & uiInventory .~ Nothing
      & uiShowFPS .~ False
      & uiShowZero .~ True
      & lgTicksPerSecond .~ initLgTicksPerSecond
      & resetWithREPLForm (mkReplForm $ mkCmdPrompt "")
      & uiReplHistory %~ restartREPLHistory
      & scenarioRef ?~ siPair
