-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.TUI.Model
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Application state for the @brick@-based Swarm TUI.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.TUI.Model
  ( -- * Custom UI label types
    -- $uilabel

    AppEvent(..), Name(..), Modal(..)

    -- * UI state

  , REPLHistItem(..)
  , InventoryEntry(..), _Separator, _InventoryEntry
  , UIState

    -- ** Fields

  , uiFocusRing, uiReplForm, uiReplType, uiReplHistory, uiReplHistIdx, uiReplLast
  , uiInventory, uiError, uiModal, lgTicksPerSecond
  , lastFrameTime, accumulatedTime, tickCount, frameCount, lastInfoTime
  , uiShowFPS, uiTPF, uiFPS

    -- ** Initialization

  , initFocusRing
  , replPrompt
  , initReplForm
  , initLgTicksPerSecond
  , initUIState

    -- ** Updating

  , populateInventoryList

    -- * App state
  , AppState
    -- ** Fields
  , gameState, uiState
    -- ** Initialization
  , initAppState

  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.List            (findIndex, sortOn)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Vector          as V
import           System.Clock
import           Text.Read            (readMaybe)

import           Brick
import           Brick.Focus
import           Brick.Forms
import qualified Brick.Widgets.List   as BL

import           Swarm.Game.Entity
import           Swarm.Game.Robot
import           Swarm.Game.State
import           Swarm.Language.Types
import           Swarm.Util

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
  = REPLPanel      -- ^ The panel containing the REPL.
  | WorldPanel     -- ^ The panel containing the world view.
  | InfoPanel      -- ^ The info panel on the left side.
  | REPLInput      -- ^ The REPL input form.
  | WorldCache     -- ^ The render cache for the world view.
  | WorldExtent    -- ^ The cached extent for the world view.
  | InventoryList  -- ^ The list of inventory items for the currently
                   --   focused robot.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Modal
  = HelpModal
  deriving (Eq, Show)

------------------------------------------------------------
-- UI state
------------------------------------------------------------

-- | An item in the REPL history.
data REPLHistItem
  = REPLEntry Bool Text    -- ^ Something entered by the user.  The
                           --   @Bool@ indicates whether it is
                           --   something entered this session (it
                           --   will be @False@ for entries that were
                           --   loaded from the history file). This is
                           --   so we know which ones to append to the
                           --   history file on shutdown.
  | REPLOutput Text        -- ^ A response printed by the system.
  deriving (Eq, Ord, Show, Read)

-- | An entry in the inventory list displayed in the info panel.  We
--   can either have an entity with a count, or a labelled separator.
--   The purpose of the separators is to show a clear distinction
--   between the robot's /inventory/ and its /installed devices/.
data InventoryEntry
  = Separator Text
  | InventoryEntry Count Entity

makePrisms ''InventoryEntry

-- | The main record holding the UI state.  For access to the fields,
-- see the lenses below.
data UIState = UIState
  { _uiFocusRing      :: FocusRing Name
  , _uiReplForm       :: Form Text AppEvent Name
  , _uiReplType       :: Maybe Polytype
  , _uiReplLast       :: Text
  , _uiReplHistory    :: [REPLHistItem]
  , _uiReplHistIdx    :: Int
  , _uiInventory      :: Maybe (Int, BL.List Name InventoryEntry)
  , _uiError          :: Maybe (Widget Name)
  , _uiModal          :: Maybe Modal
  , _uiShowFPS        :: Bool
  , _uiTPF            :: Double
  , _uiFPS            :: Double
  , _lgTicksPerSecond :: Int
  , _tickCount        :: Int
  , _frameCount       :: Int
  , _lastFrameTime    :: TimeSpec
  , _accumulatedTime  :: TimeSpec
  , _lastInfoTime     :: TimeSpec
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''UIState

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
uiReplHistory :: Lens' UIState [REPLHistItem]

-- | The current index in the REPL history (if the user is going back
--   through the history using up/down keys).
uiReplHistIdx :: Lens' UIState Int

-- | The hash value of the focused robot entity (so we can tell if its
--   inventory changed) along with a list of the items in the
--   focused robot's inventory.
uiInventory :: Lens' UIState (Maybe (Int, BL.List Name InventoryEntry))

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

-- | The base-2 logarithm of the current game speed in ticks per
--   second.
lgTicksPerSecond :: Lens' UIState Int

-- | A counter used to track how many ticks happen in a single frame.
tickCount :: Lens' UIState Int

-- | A counter used to track how many frame got rendered
frameCount :: Lens' UIState Int

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
initFocusRing = focusRing [REPLPanel, InfoPanel, WorldPanel]

-- | The default REPL prompt.
replPrompt :: Text
replPrompt = "> "

-- | The initial state of the REPL entry form.
initReplForm :: Form Text AppEvent Name
initReplForm = newForm
  [(txt replPrompt <+>) @@= editTextField id REPLInput (Just 1)]
  ""

-- | The initial tick speed.
initLgTicksPerSecond :: Int
initLgTicksPerSecond = 3  -- 2^3 = 8 ticks / second

-- | Initialize the UI state.  This needs to be in the IO monad since
--   it involves reading a REPL history file and getting the current
--   time.
initUIState :: ExceptT Text IO UIState
initUIState = liftIO $ do
  mhist <- (>>= readMaybe @[REPLHistItem]) <$> readFileMay ".swarm_history"
  startTime <- getTime Monotonic
  return $ UIState
    { _uiFocusRing      = initFocusRing
    , _uiReplForm       = initReplForm
    , _uiReplType       = Nothing
    , _uiReplHistory    = mhist ? []
    , _uiReplHistIdx    = -1
    , _uiReplLast       = ""
    , _uiInventory      = Nothing
    , _uiError          = Nothing
    , _uiModal          = Nothing
    , _uiShowFPS        = False
    , _uiTPF            = 0
    , _uiFPS            = 0
    , _lgTicksPerSecond = initLgTicksPerSecond
    , _lastFrameTime    = startTime
    , _accumulatedTime  = 0
    , _lastInfoTime     = 0
    , _tickCount        = 0
    , _frameCount       = 0
    }

------------------------------------------------------------
-- Functions for updating the UI state
------------------------------------------------------------

-- | Given the focused robot, populate the UI inventory list in the info
--   panel with information about its inventory.
populateInventoryList :: MonadState UIState m => Maybe Robot -> m ()
populateInventoryList Nothing  = uiInventory .= Nothing
populateInventoryList (Just r) = do
  mList <- preuse (uiInventory . _Just . _2)
  let mkInvEntry (n,e) = InventoryEntry n e
      itemList label
        = (\case { [] -> []; xs -> Separator label : xs })
        . map mkInvEntry
        . sortOn (view entityName . snd)
        . elems
      items = (r ^. robotInventory . to (itemList "Inventory"))
           ++ (r ^. installedDevices . to (itemList "Installed devices"))

      -- Attempt to keep the selected element steady.
      sel = mList >>= BL.listSelectedElement  -- Get the currently selected element+index.
      idx = case sel of
        -- If there is no currently selected element, just focus on
        -- index 1 (not 0, to avoid the separator).
        Nothing -> 1
        -- Otherwise, try to find the same entity in the list and focus on that;
        -- if it's not there, keep the index the same.
        Just (selIdx, InventoryEntry _ e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview (_InventoryEntry . _2)) items)
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
  , _uiState   :: UIState
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''AppState

-- | The 'GameState' record.
gameState :: Lens' AppState GameState

-- | The 'UIState' record.
uiState :: Lens' AppState UIState

-- | Initialize the 'AppState'.
initAppState :: ExceptT Text IO AppState
initAppState = AppState <$> initGameState <*> initUIState


------------------------------------------------------------
--
