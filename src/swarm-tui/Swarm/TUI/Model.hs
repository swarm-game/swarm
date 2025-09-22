{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Application state for the @brick@-based Swarm TUI.
module Swarm.TUI.Model (
  -- * Custom UI label types
  -- $uilabel
  AppEvent (..),
  FocusablePanel (..),
  Name (..), -- helps to minimize import lines

  -- ** Web command
  WebCommand (..),
  WebInvocationState (..),
  RejectionReason (..),

  -- * Menus and dialogs
  ModalType (..),
  ScenarioOutcome (..),
  Button (..),
  ButtonAction (..),
  modalType,
  modalDialog,
  MainMenuEntry (..),
  mainMenu,
  _NewGameMenu,
  mkScenarioList,

  -- * UI state

  -- ** Inventory
  InventoryListEntry (..),
  _Separator,
  _InventoryEntry,
  _EquippedEntry,

  -- ** Updating
  populateInventoryList,
  infoScroll,
  modalScroll,
  replScroll,

  -- ** Utility
  logEvent,
  SwarmKeyDispatcher,
  KeyEventHandlingState (KeyEventHandlingState),
  SwarmKeyDispatchers (..),
  keyConfig,
  keyDispatchers,

  -- * App state
  AppState (AppState),
  uiState,
  playState,
  keyEventHandling,
  runtimeState,
  animationMgr,
  ScenarioState (ScenarioState),
  gameState,
  uiGameplay,
  PlayState (..),
  scenarioState,
  progression,
  ProgressionState (..),
  scenarios,
  attainedAchievements,
  uiPopups,
  uiPopupAnimationState,
  scenarioSequence,
  AnimationState (..),
  _AnimActive,
  _AnimScheduled,
  _AnimInactive,

  -- ** Initialization
  AppOpts (..),
  defaultAppOpts,
  RunOpts (..),
  _AutoPlay,
  _RunScript,
  _Replay,

  -- *** Re-exported types used in options
  ColorMode (..),

  -- ** Utility
  focusedItem,
  focusedEntity,
  animTraversal,
) where

import Brick (EventM, ViewportScroll, viewportScroll)
import Brick.Animation (Animation, AnimationManager)
import Brick.Keybindings as BK
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (<.>))
import Control.Monad ((>=>))
import Control.Monad.State (MonadState)
import Data.List (findIndex)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GitHash (GitInfo)
import Graphics.Vty (ColorMode (..))
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Entity as E
import Swarm.Game.Ingredients
import Swarm.Game.Popup
import Swarm.Game.Robot
import Swarm.Game.Scenario.Status
import Swarm.Game.ScenarioInfo (ScenarioCollection)
import Swarm.Game.State
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Game.World (Seed)
import Swarm.Language.Text.Markdown qualified as Markdown
import Swarm.Log
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Model.DebugOption (DebugOption)
import Swarm.TUI.Model.Event (SwarmEvent)
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.Model.WebCommand (RejectionReason (..), WebCommand (..), WebInvocationState (..))
import Swarm.Util.Lens (makeLensesNoSigs)
import Text.Fuzzy qualified as Fuzzy

------------------------------------------------------------
-- Custom UI label types
------------------------------------------------------------

-- $uilabel These types are used as parameters to various @brick@
-- types.

-- | 'Swarm.TUI.Model.AppEvent' represents a type for custom event types our app can
--   receive. The primary custom event 'Frame' is sent by a separate thread as fast as
--   it can, telling the TUI to render a new frame. The custom event 'PopupEvent' is sent
--   by the animation manager and contains an event that starts, stops, or updates a
--   popup notification.
data AppEvent
  = Frame
  | Web WebCommand
  | PopupEvent (EventM Name AppState ())
  | UpstreamVersion (Either (Severity, Text) String)

infoScroll :: ViewportScroll Name
infoScroll = viewportScroll InfoViewport

modalScroll :: ViewportScroll Name
modalScroll = viewportScroll ModalViewport

replScroll :: ViewportScroll Name
replScroll = viewportScroll REPLViewport

--------------------------------------------------
-- Utility

-- | Simply log to the runtime event log.
logEvent :: LogSource -> Severity -> Text -> Text -> Notifications LogEntry -> Notifications LogEntry
logEvent src sev who msg el =
  el
    & notificationsCount %~ succ
    & notificationsContent %~ (l :)
 where
  l = LogEntry (TickNumber 0) src sev who msg

-- | This encapsulates both game and UI state for an actively-playing scenario, as well
-- as state that evolves as a result of playing a scenario.
data ScenarioState = ScenarioState
  { _gameState :: GameState
  , _uiGameplay :: UIGameplay
  }

-- | This enapsulates the state of a given animation that changes over time. 'AnimInactive' means that
--   the application is ready to start a new animation. 'AnimScheduled' means that the application
--   has told the animation manager to start the animation, but it hasn't started yet. 'AnimActive' means
--   that the animation is currently in progress.
data AnimationState
  = AnimActive (Animation AppState Name)
  | AnimScheduled
  | AnimInactive

-- | State that can evolve as the user progresses through scenarios.
-- This includes achievements and completion records.
--
-- Note that scenario completion/achievements are serialized to disk storage,
-- but we also persist in memory since we don't reload data from disk as
-- we progress through scenarios.
data ProgressionState = ProgressionState
  { _scenarios :: ScenarioCollection ScenarioInfo
  , _attainedAchievements :: Map CategorizedAchievement Attainment
  , _uiPopups :: PopupState
  , _uiPopupAnimationState :: AnimationState
  , _scenarioSequence :: [ScenarioWith ScenarioPath]
  }

-- | This encapsulates both game and UI state for an actively-playing scenario, as well
-- as state that evolves as a result of playing a scenario.
data PlayState = PlayState
  { _scenarioState :: ScenarioState
  , _progression :: ProgressionState
  }

-- ----------------------------------------------------------------------------
--                                   APPSTATE                                --
-- ----------------------------------------------------------------------------

-- | The 'AppState' just stores together the other states.
--
-- This is so you can use a smaller state when e.g. writing some game logic
-- or updating the UI. Also consider that GameState can change when loading
-- a new scenario - if the state should persist games, use RuntimeState.
data AppState = AppState
  { _playState :: PlayState
  , _uiState :: UIState
  , _keyEventHandling :: KeyEventHandlingState
  , _runtimeState :: RuntimeState
  , _animationMgr :: AnimationManager AppState AppEvent Name
  }

------------------------------------------------------------

type SwarmKeyDispatcher = KeyDispatcher SwarmEvent (EventM Name AppState)

data SwarmKeyDispatchers = SwarmKeyDispatchers
  { mainGameDispatcher :: SwarmKeyDispatcher
  , replDispatcher :: SwarmKeyDispatcher
  , worldDispatcher :: SwarmKeyDispatcher
  , robotDispatcher :: SwarmKeyDispatcher
  }

data KeyEventHandlingState = KeyEventHandlingState
  { _keyConfig :: KeyConfig SwarmEvent
  , _keyDispatchers :: SwarmKeyDispatchers
  }

------------------------------------------------------------
-- Functions for updating the UI state
------------------------------------------------------------

-- | Given the focused robot, populate the UI inventory list in the info
--   panel with information about its inventory.
populateInventoryList :: (MonadState UIInventory m) => Maybe Robot -> m ()
populateInventoryList Nothing = uiInventoryList .= Nothing
populateInventoryList (Just r) = do
  mList <- preuse $ uiInventoryList . _Just . _2
  showZero <- use uiShowZero
  sortOptions <- use uiInventorySort
  search <- use uiInventorySearch
  let mkInvEntry (n, e) = InventoryEntry n e
      mkInstEntry (_, e) = EquippedEntry e
      itemList isInventoryDisplay mk label =
        (\case [] -> []; xs -> Separator label : xs)
          . map mk
          . sortInventory sortOptions
          . filter ((&&) <$> matchesSearch <*> shouldDisplay)
          . elems
       where
        -- Display items if we have a positive number of them, or they
        -- aren't an equipped device.  In other words we don't need to
        -- display equipped devices twice unless we actually have some
        -- in our inventory in addition to being equipped.
        shouldDisplay (n, e) =
          n > 0
            || isInventoryDisplay
              && showZero
              && not ((r ^. equippedDevices) `E.contains` e)

      matchesSearch :: (Count, Entity) -> Bool
      matchesSearch (_, e) =
        or
          [ -- Fuzzy search within entity names.
            maybe (const True) Fuzzy.test search (e ^. E.entityName)
          , -- Also do a literal substring search within entity
            -- descriptions.  Since descriptions are long, a fuzzy
            -- search tends to yield too many false positives.
            maybe (const True) T.isInfixOf search (Markdown.docToText (e ^. E.entityDescription))
          ]

      items =
        (r ^. robotInventory . to (itemList True mkInvEntry "Compendium"))
          ++ (r ^. equippedDevices . to (itemList False mkInstEntry "Equipped devices"))

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
        Just (selIdx, EquippedEntry e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview _EquippedEntry) items)
        Just (selIdx, _) -> selIdx

      -- Create the new list, focused at the desired index.
      lst = BL.listMoveTo idx $ BL.list InventoryList (V.fromList items) 1

  -- Finally, populate the newly created list in the UI, and remember
  -- the hash of the current robot.
  uiInventoryList .= Just (r ^. inventoryHash, lst)

------------------------------------------------------------
-- App state (= UI state + game state) initialization
------------------------------------------------------------

-- | Command-line options for configuring the app.
data AppOpts = AppOpts
  { userSeed :: Maybe Seed
  -- ^ Explicit seed chosen by the user.
  , userScenario :: Maybe FilePath
  -- ^ Scenario the user wants to play.
  , runOpts :: Maybe RunOpts
  -- ^ Code to be run on base.
  , pausedAtStart :: Bool
  -- ^ Pause the game on start by default.
  , autoShowObjectives :: Bool
  -- ^ Show objectives dialogs when an objective is achieved/failed.
  , speed :: Int
  -- ^ Initial game speed (logarithm)
  , debugOptions :: Set DebugOption
  -- ^ Debugging options, for example show creative switch.
  , colorMode :: Maybe ColorMode
  -- ^ What colour mode should be used?
  , userWebPort :: Maybe Port
  -- ^ Explicit port on which to run the web API
  , userMetricsPort :: Maybe Port
  -- ^ Explicit port on which to run the web API
  , repoGitInfo :: Maybe GitInfo
  -- ^ Information about the Git repository (not present in release).
  }

data RunOpts
  = -- | Automatically run the solution defined in the scenario file.
    AutoPlay
  | -- | Swarm script to be run on base.
    RunScript FilePath
  | -- | Previous JSON REPL history to replay.
    Replay FilePath

makePrisms ''RunOpts

-- | A default/empty 'AppOpts' record.
defaultAppOpts :: AppOpts
defaultAppOpts =
  AppOpts
    { userSeed = Nothing
    , userScenario = Nothing
    , runOpts = Nothing
    , pausedAtStart = False
    , autoShowObjectives = True
    , speed = defaultInitLgTicksPerSecond
    , debugOptions = mempty
    , colorMode = Nothing
    , userWebPort = Nothing
    , userMetricsPort = Nothing
    , repoGitInfo = Nothing
    }

--------------------------------------------------
-- Lenses for ScenarioState

makeLensesNoSigs ''ScenarioState

-- | The 'GameState' record.
gameState :: Lens' ScenarioState GameState

-- | UI active during live gameplay
uiGameplay :: Lens' ScenarioState UIGameplay

--------------------------------------------------
-- Lenses for PlayState

makeLensesNoSigs ''PlayState

-- | The 'ScenarioState' record.
scenarioState :: Lens' PlayState ScenarioState

-- | State that can evolve as the user progresses through scenarios.
progression :: Lens' PlayState ProgressionState

--------------------------------------------------
-- Lenses for Progression State
makeLensesNoSigs ''ProgressionState

-- | Map of achievements that were attained
attainedAchievements :: Lens' ProgressionState (Map CategorizedAchievement Attainment)

-- | The collection of scenarios that comes with the game.
scenarios :: Lens' ProgressionState (ScenarioCollection ScenarioInfo)

-- | Queue of popups to display
uiPopups :: Lens' ProgressionState PopupState

-- | Popup Animation State
uiPopupAnimationState :: Lens' ProgressionState AnimationState

-- | Remaining scenarios in the current sequence
scenarioSequence :: Lens' ProgressionState [ScenarioWith ScenarioPath]

--------------------------------------------------
-- Lenses for KeyEventHandlingState

makeLensesNoSigs ''KeyEventHandlingState

-- | Keybindings (possibly customized by player) for 'SwarmEvent's.
keyConfig :: Lens' KeyEventHandlingState (KeyConfig SwarmEvent)

-- | Dispatchers that will call handler on key combo.
keyDispatchers :: Lens' KeyEventHandlingState SwarmKeyDispatchers

--------------------------------------------------
-- Lenses for AppState

makeLensesNoSigs ''AppState

-- | The 'ScenarioState' record.
playState :: Lens' AppState PlayState

-- | The 'UIState' record.
uiState :: Lens' AppState UIState

-- | The key event handling configuration.
keyEventHandling :: Lens' AppState KeyEventHandlingState

-- | The 'RuntimeState' record
runtimeState :: Lens' AppState RuntimeState

-- | The 'Brick.Animation.AnimationManager' record
animationMgr :: Lens' AppState (AnimationManager AppState AppEvent Name)

-------------------------------------------------

-- | Prisms for AnimationState
makePrisms ''AnimationState

--------------------------------------------------
-- Utility functions

-- | Get the currently focused 'InventoryListEntry' from the robot
--   info panel (if any).
focusedItem :: ScenarioState -> Maybe InventoryListEntry
focusedItem s = do
  list <- s ^? uiGameplay . uiInventory . uiInventoryList . _Just . _2
  (_, entry) <- BL.listSelectedElement list
  return entry

-- | Get the currently focused entity from the robot info panel (if
--   any).  This is just like 'focusedItem' but forgets the
--   distinction between plain inventory items and equipped devices.
focusedEntity :: ScenarioState -> Maybe Entity
focusedEntity =
  focusedItem >=> \case
    Separator _ -> Nothing
    InventoryEntry _ e -> Just e
    EquippedEntry e -> Just e

-- | A non-lawful traversal for use in animations that allows
--   us to manage the state of an animation and update it properly
--   when we process an event sent by the animation manager.
--   Exploits some assumptions about Brick's implementation of animations.
--   It is defined such that when the animation manager starts the animation
--   by setting the target of the traversal to Just theAnimation, the traversal will actually
--   set the AnimationState of the popup animation to AnimActive theAnimation.
--   When the animation manager signals that the animation has stopped by setting the target of
--   the traversal to Nothing, the traversal will set the AnimationState of the popup to AnimInactive.
animTraversal :: Traversal' AnimationState (Maybe (Animation AppState Name))
animTraversal = traversal go
 where
  go :: Applicative f => (Maybe (Animation AppState Name) -> f (Maybe (Animation AppState Name))) -> AnimationState -> f AnimationState
  go focus = \case
    AnimInactive -> maybe AnimInactive AnimActive <$> focus Nothing
    AnimScheduled -> maybe AnimInactive AnimActive <$> focus Nothing
    AnimActive x -> maybe AnimInactive AnimActive <$> focus (Just x)
