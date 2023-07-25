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
  WebCommand (..),
  FocusablePanel (..),
  Name (..),

  -- * Menus and dialogs
  ModalType (..),
  ScenarioOutcome (..),
  Button (..),
  ButtonAction (..),
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
  removeEntry,

  -- ** Inventory
  InventoryListEntry (..),
  _Separator,
  _InventoryEntry,
  _EquippedEntry,

  -- *** REPL Panel Model
  REPLState,
  ReplControlMode (..),
  replPromptType,
  replPromptEditor,
  replPromptText,
  replValid,
  replLast,
  replType,
  replControlMode,
  replHistory,
  newREPLEditor,

  -- ** Updating
  populateInventoryList,
  infoScroll,
  modalScroll,

  -- * Runtime state
  RuntimeState,
  webPort,
  upstreamRelease,
  eventLog,
  scenarios,
  stdEntityMap,
  stdRecipes,
  appData,
  stdAdjList,
  stdNameList,

  -- ** Utility
  logEvent,
  mkGameStateConfig,

  -- * App state
  AppState (AppState),
  gameState,
  uiState,
  runtimeState,

  -- ** Initialization
  AppOpts (..),
  defaultAppOpts,
  Seed,

  -- *** Re-exported types used in options
  ColorMode (..),

  -- ** Utility
  topContext,
  focusedItem,
  focusedEntity,
  nextScenario,
  initRuntimeState,
) where

import Brick
import Brick.Widgets.List qualified as BL
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens hiding (from, (<.>))
import Control.Monad ((>=>))
import Control.Monad.State (MonadState)
import Data.Array (Array, listArray)
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T (lines)
import Data.Vector qualified as V
import GitHash (GitInfo)
import Graphics.Vty (ColorMode (..))
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.CESK (TickNumber (..))
import Swarm.Game.Entity as E
import Swarm.Game.Failure
import Swarm.Game.Recipe (Recipe, loadRecipes)
import Swarm.Game.ResourceLoading (readAppData)
import Swarm.Game.Robot
import Swarm.Game.Scenario.Status
import Swarm.Game.ScenarioInfo (ScenarioCollection, loadScenarios, _SISingle)
import Swarm.Game.State
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Version (NewReleaseFailure (NoMainUpstreamRelease))
import System.FilePath ((<.>))
import Text.Fuzzy qualified as Fuzzy
import Witch (into)

------------------------------------------------------------
-- Custom UI label types
------------------------------------------------------------

-- $uilabel These types are used as parameters to various @brick@
-- types.

newtype WebCommand = RunWebCode Text
  deriving (Show)

-- | 'Swarm.TUI.Model.AppEvent' represents a type for custom event types our app can
--   receive. The primary custom event 'Frame' is sent by a separate thread as fast as
--   it can, telling the TUI to render a new frame.
data AppEvent
  = Frame
  | Web WebCommand
  | UpstreamVersion (Either NewReleaseFailure String)
  deriving (Show)

infoScroll :: ViewportScroll Name
infoScroll = viewportScroll InfoViewport

modalScroll :: ViewportScroll Name
modalScroll = viewportScroll ModalViewport

-- ----------------------------------------------------------------------------
--                                Runtime state                              --
-- ----------------------------------------------------------------------------

data RuntimeState = RuntimeState
  { _webPort :: Maybe Port
  , _upstreamRelease :: Either NewReleaseFailure String
  , _eventLog :: Notifications LogEntry
  , _scenarios :: ScenarioCollection
  , _stdEntityMap :: EntityMap
  , _stdRecipes :: [Recipe Entity]
  , _appData :: Map Text Text
  , _stdAdjList :: Array Int Text
  , _stdNameList :: Array Int Text
  }

initRuntimeState ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  m RuntimeState
initRuntimeState = do
  entities <- loadEntities
  recipes <- loadRecipes entities
  scenarios <- loadScenarios entities
  appDataMap <- readAppData

  let getDataLines f = case M.lookup f appDataMap of
        Nothing ->
          throwError $
            AssetNotLoaded (Data NameGeneration) (into @FilePath f <.> ".txt") (DoesNotExist File)
        Just content -> return . tail . T.lines $ content
  adjs <- getDataLines "adjectives"
  names <- getDataLines "names"

  return $
    RuntimeState
      { _webPort = Nothing
      , _upstreamRelease = Left (NoMainUpstreamRelease [])
      , _eventLog = mempty
      , _scenarios = scenarios
      , _stdEntityMap = entities
      , _stdRecipes = recipes
      , _appData = appDataMap
      , _stdAdjList = listArray (0, length adjs - 1) adjs
      , _stdNameList = listArray (0, length names - 1) names
      }

makeLensesNoSigs ''RuntimeState

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

-- | The collection of scenarios that comes with the game.
scenarios :: Lens' RuntimeState ScenarioCollection

-- | The standard entity map loaded from disk.  Individual scenarios
--   may define additional entities which will get added to this map
--   when loading the scenario.
stdEntityMap :: Lens' RuntimeState EntityMap

-- | The standard list of recipes loaded from disk.  Individual scenarios
--   may define additional recipes which will get added to this list
--   when loading the scenario.
stdRecipes :: Lens' RuntimeState [Recipe Entity]

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' RuntimeState (Map Text Text)

-- | List of words for use in building random robot names.
stdAdjList :: Lens' RuntimeState (Array Int Text)

-- | List of words for use in building random robot names.
stdNameList :: Lens' RuntimeState (Array Int Text)

--------------------------------------------------
-- Utility

-- | Simply log to the runtime event log.
logEvent :: LogSource -> (Text, RID) -> Text -> Notifications LogEntry -> Notifications LogEntry
logEvent src (who, rid) msg el =
  el
    & notificationsCount %~ succ
    & notificationsContent %~ (l :)
 where
  l = LogEntry (TickNumber 0) src who rid Omnipresent msg

-- | Create a 'GameStateConfig' record from the 'RuntimeState'.
mkGameStateConfig :: RuntimeState -> GameStateConfig
mkGameStateConfig rs =
  GameStateConfig
    { initAdjList = rs ^. stdAdjList
    , initNameList = rs ^. stdNameList
    , initEntities = rs ^. stdEntityMap
    , initRecipes = rs ^. stdRecipes
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
  { _gameState :: GameState
  , _uiState :: UIState
  , _runtimeState :: RuntimeState
  }

--------------------------------------------------
-- Lenses for AppState

makeLensesNoSigs ''AppState

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
--   distinction between plain inventory items and equipped devices.
focusedEntity :: AppState -> Maybe Entity
focusedEntity =
  focusedItem >=> \case
    Separator _ -> Nothing
    InventoryEntry _ e -> Just e
    EquippedEntry e -> Just e

------------------------------------------------------------
-- Functions for updating the UI state
------------------------------------------------------------

-- | Given the focused robot, populate the UI inventory list in the info
--   panel with information about its inventory.
populateInventoryList :: (MonadState UIState m) => Maybe Robot -> m ()
populateInventoryList Nothing = uiInventory .= Nothing
populateInventoryList (Just r) = do
  mList <- preuse (uiInventory . _Just . _2)
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
      matchesSearch (_, e) = maybe (const True) Fuzzy.test search (e ^. E.entityName)

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
  uiInventory .= Just (r ^. inventoryHash, lst)

------------------------------------------------------------
-- App state (= UI state + game state) initialization
------------------------------------------------------------

-- | Command-line options for configuring the app.
data AppOpts = AppOpts
  { userSeed :: Maybe Seed
  -- ^ Explicit seed chosen by the user.
  , userScenario :: Maybe FilePath
  -- ^ Scenario the user wants to play.
  , scriptToRun :: Maybe FilePath
  -- ^ Code to be run on base.
  , autoPlay :: Bool
  -- ^ Automatically run the solution defined in the scenario file
  , speed :: Int
  -- ^ Initial game speed (logarithm)
  , cheatMode :: Bool
  -- ^ Should cheat mode be enabled?
  , colorMode :: Maybe ColorMode
  -- ^ What colour mode should be used?
  , userWebPort :: Maybe Port
  -- ^ Explicit port on which to run the web API
  , repoGitInfo :: Maybe GitInfo
  -- ^ Information about the Git repository (not present in release).
  }

-- | A default/empty 'AppOpts' record.
defaultAppOpts :: AppOpts
defaultAppOpts =
  AppOpts
    { userSeed = Nothing
    , userScenario = Nothing
    , scriptToRun = Nothing
    , autoPlay = False
    , speed = defaultInitLgTicksPerSecond
    , cheatMode = False
    , colorMode = Nothing
    , userWebPort = Nothing
    , repoGitInfo = Nothing
    }

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

-- | Context for the REPL commands to execute in. Contains the base
--   robot context plus the `it` variable that refer to the previously
--   computed values. (Note that `it{n}` variables are set in the
--   base robot context; we only set `it` here because it's so transient)
topContext :: AppState -> RobotContext
topContext s = ctxPossiblyWithIt
 where
  ctx = fromMaybe emptyRobotContext $ s ^? gameState . baseRobot . robotContext

  ctxPossiblyWithIt = case s ^. gameState . replStatus of
    REPLDone (Just p) -> ctx & at "it" ?~ p
    _ -> ctx
