{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
  Modal (..),

  -- * UI state

  -- ** REPL
  REPLHistItem (..),
  firstReplEntry,
  replItemText,
  isREPLEntry,
  getREPLEntry,
  REPLHistory,
  replSeq,
  replStart,
  replIndex,
  addREPLEntry,
  addREPLOutput,
  replLength,

  -- ** Inventory
  InventoryListEntry (..),
  _Separator,
  _InventoryEntry,
  _InstalledEntry,

  -- ** UI Model
  UIState,
  uiFocusRing,
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
  lgTicksPerSecond,
  lastFrameTime,
  accumulatedTime,
  tickCount,
  frameCount,
  frameTickCount,
  lastInfoTime,
  uiShowFPS,
  uiTPF,
  uiFPS,

  -- ** Initialization
  initFocusRing,
  replPrompt,
  initReplForm,
  initLgTicksPerSecond,
  initUIState,

  -- ** Updating
  populateInventoryList,
  infoScroll,

  -- * App state
  AppState,

  -- ** Fields
  gameState,
  uiState,

  -- ** Initialization
  initAppState,
  Seed,
) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.List (findIndex, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Clock

import Brick
import Brick.Focus
import Brick.Forms
import qualified Brick.Widgets.List as BL

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Swarm.Game.Entity as E
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Language.Types
import Swarm.Util

------------------------------------------------------------
-- Custom UI label types
------------------------------------------------------------

-- $uilabel These types are used as parameters to various @brick@
-- types.

-- | 'Swarm.TUI.Model.AppEvent' represents a type for custom event types our app can
--   receive.  At the moment, we only have one custom event, but it's
--   very important: a separate thread sends 'Frame' events as fast as
--   it can, telling the TUI to render a new frame.
data AppEvent = Frame
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
  | -- | The scrollable viewport for the info panel.
    InfoViewport
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

infoScroll :: ViewportScroll Name
infoScroll = viewportScroll InfoViewport

data Modal
  = HelpModal
  deriving (Eq, Show)

------------------------------------------------------------
-- UI state
------------------------------------------------------------

-- | An item in the REPL history.
data REPLHistItem
  = -- | Something entered by the user.
    REPLEntry Text
  | -- | A response printed by the system.
    REPLOutput Text
  deriving (Eq, Ord, Show, Read)

getREPLEntry :: REPLHistItem -> Maybe Text
getREPLEntry = \case
  REPLEntry t -> Just t
  _ -> Nothing

isREPLEntry :: REPLHistItem -> Bool
isREPLEntry = isJust . getREPLEntry

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
--
-- TODO: The ideal would be not to expose inner storage, but currently
-- namely the index+predicate functions on Seq are hard to resist.
replSeq :: Lens' REPLHistory (Seq REPLHistItem)

-- | The current index in the REPL history (if the user is going back
--   through the history using up/down keys).
replIndex :: Lens' REPLHistory Int

-- | The index of the first entry since loading saved history.
replStart :: Lens' REPLHistory Int

-- | Current (vertical) length of the REPL - used as index of input buffer.
replLength :: REPLHistory -> Int
replLength = length . _replSeq

-- | Add new REPL input - the index must have been pointing one past
--   the last element already, so we increment it to keep it that way.
addREPLItem :: (Text -> REPLHistItem) -> Text -> REPLHistory -> REPLHistory
addREPLItem rc t h =
  h
    & replSeq %~ (|> rc t)
    & replIndex .~ replLength h

-- | Add new REPL input and increment index to last.
addREPLEntry :: Text -> REPLHistory -> REPLHistory
addREPLEntry = addREPLItem REPLEntry

-- | Add new REPL output and increment index to last.
addREPLOutput :: Text -> REPLHistory -> REPLHistory
addREPLOutput = addREPLItem REPLOutput

-- | Given a REPL history return @Just@ the most recent @Text@
--   entered by the user or @Nothing@ if there is none.
firstReplEntry :: REPLHistory -> Maybe Text
firstReplEntry = foldr (const . getREPLEntry) Nothing . _replSeq

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

-- | The main record holding the UI state.  For access to the fields,
-- see the lenses below.
data UIState = UIState
  { _uiFocusRing :: FocusRing Name
  , _uiReplForm :: Form Text AppEvent Name
  , _uiReplType :: Maybe Polytype
  , _uiReplLast :: Text
  , _uiReplHistory :: REPLHistory
  , _uiInventory :: Maybe (Int, BL.List Name InventoryListEntry)
  , _uiMoreInfoTop :: Bool
  , _uiMoreInfoBot :: Bool
  , _uiScrollToEnd :: Bool
  , _uiError :: Maybe (Widget Name)
  , _uiModal :: Maybe Modal
  , _uiShowFPS :: Bool
  , _uiTPF :: Double
  , _uiFPS :: Double
  , _lgTicksPerSecond :: Int
  , _tickCount :: Int
  , _frameCount :: Int
  , _frameTickCount :: Int
  , _lastFrameTime :: TimeSpec
  , _accumulatedTime :: TimeSpec
  , _lastInfoTime :: TimeSpec
  }

let exclude = ['_lgTicksPerSecond]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''UIState

-- | The focus ring is the set of UI panels we can cycle among using
--   the Tab key.
uiFocusRing :: Lens' UIState (FocusRing Name)

-- | The form where the user can type input at the REPL.
uiReplForm :: Lens' UIState (Form Text AppEvent Name)

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
uiError :: Lens' UIState (Maybe (Widget Name))

-- | When this is @Just@, it represents a modal to be displayed on
--   top of the UI, e.g. for the Help screen.
uiModal :: Lens' UIState (Maybe Modal)

-- | A togle to show the FPS by pressing `f`
uiShowFPS :: Lens' UIState Bool

-- | Computed ticks per milli seconds
uiTPF :: Lens' UIState Double

-- | Computed frames per milli seconds
uiFPS :: Lens' UIState Double

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

-- | The initial state of the focus ring.
initFocusRing :: FocusRing Name
initFocusRing = focusRing [REPLPanel, InfoPanel, RobotPanel, WorldPanel]

-- | The default REPL prompt.
replPrompt :: Text
replPrompt = "> "

-- | The initial state of the REPL entry form.
initReplForm :: Form Text AppEvent Name
initReplForm =
  newForm
    [(txt replPrompt <+>) @@= editTextField id REPLInput (Just 1)]
    ""

-- | The initial tick speed.
initLgTicksPerSecond :: Int
initLgTicksPerSecond = 3 -- 2^3 = 8 ticks / second

-- | Initialize the UI state.  This needs to be in the IO monad since
--   it involves reading a REPL history file and getting the current
--   time.
initUIState :: ExceptT Text IO UIState
initUIState = liftIO $ do
  historyT <- readFileMayT =<< getSwarmHistoryPath False
  let history = maybe Seq.empty (Seq.fromList . map REPLEntry . T.lines) historyT
      historyLen = length history
  startTime <- getTime Monotonic
  return $
    UIState
      { _uiFocusRing = initFocusRing
      , _uiReplForm = initReplForm
      , _uiReplType = Nothing
      , _uiReplHistory =
          REPLHistory
            { _replSeq = history
            , _replIndex = historyLen -- one past rightmost=newest
            , _replStart = historyLen
            }
      , _uiReplLast = ""
      , _uiInventory = Nothing
      , _uiMoreInfoTop = False
      , _uiMoreInfoBot = False
      , _uiScrollToEnd = False
      , _uiError = Nothing
      , _uiModal = Nothing
      , _uiShowFPS = False
      , _uiTPF = 0
      , _uiFPS = 0
      , _lgTicksPerSecond = initLgTicksPerSecond
      , _lastFrameTime = startTime
      , _accumulatedTime = 0
      , _lastInfoTime = 0
      , _tickCount = 0
      , _frameCount = 0
      , _frameTickCount = 0
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
      shouldDisplay (n, e) = n > 0 || not ((r ^. installedDevices) `E.contains` e)

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

------------------------------------------------------------
-- App state (= UI state + game state)
------------------------------------------------------------

-- | The 'AppState' just stores together the game state and UI state.
data AppState = AppState
  { _gameState :: GameState
  , _uiState :: UIState
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''AppState

-- | The 'GameState' record.
gameState :: Lens' AppState GameState

-- | The 'UIState' record.
uiState :: Lens' AppState UIState

-- | Initialize the 'AppState'.
initAppState :: Seed -> ExceptT Text IO AppState
initAppState seed = AppState <$> initGameState seed <*> initUIState

------------------------------------------------------------
--
