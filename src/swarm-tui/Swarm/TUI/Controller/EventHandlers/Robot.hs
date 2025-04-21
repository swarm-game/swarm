{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Here is the RobotPanel key event handler.
--
-- Because of how tricky the search logic is,
-- the player configurable part and the dynamic
-- search handler are both here.
module Swarm.TUI.Controller.EventHandlers.Robot (
  robotEventHandlers,
  handleRobotPanelEvent,
) where

import Brick
import Brick.Keybindings
import Control.Lens as Lens
import Control.Lens.Extras as Lens (is)
import Control.Monad (unless, when)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Robot.Concrete
import Swarm.Game.State
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax hiding (Key)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Inventory.Sorting (cycleSortDirection, cycleSortOrder)
import Swarm.TUI.List
import Swarm.TUI.Model
import Swarm.TUI.Model.Event
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Util (generateModal)

-- | Handle user input events in the robot panel.
handleRobotPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleRobotPanelEvent bev = do
  search <- use $ playState . scenarioState . uiGameplay . uiInventory . uiInventorySearch
  keyHandler <- use $ keyEventHandling . keyDispatchers . to robotDispatcher
  case search of
    Just _ -> handleInventorySearchEvent bev
    Nothing -> case bev of
      VtyEvent ev@(V.EvKey k m) -> do
        handled <- handleKey keyHandler k m
        unless handled $
          Brick.zoom (playState . scenarioState . uiGameplay) $
            handleInventoryListEvent ev
      _ -> continueWithoutRedraw

-- | Handle key events in the robot panel.
robotEventHandlers :: [KeyEventHandler SwarmEvent (EventM Name AppState)]
robotEventHandlers = nonCustomizableHandlers <> customizableHandlers
 where
  nonCustomizableHandlers =
    [ onKey V.KEnter "Show entity description" $ scenarioStateWithMenu showEntityDescription
    ]
  customizableHandlers = allHandlers Robot $ \case
    MakeEntityEvent -> ("Make the selected entity", Brick.zoom (playState . scenarioState) makeFocusedEntity)
    ShowZeroInventoryEntitiesEvent -> ("Show entities with zero count in inventory", zoomInventory showZero)
    CycleInventorySortEvent -> ("Cycle inventory sorting type", zoomInventory cycleSort)
    SwitchInventorySortDirection -> ("Switch ascending/descending inventory sort", zoomInventory switchSortDirection)
    SearchInventoryEvent -> ("Start inventory search", zoomInventory searchInventory)

-- | Display a modal window with the description of an entity.
showEntityDescription :: Menu -> EventM Name ScenarioState ()
showEntityDescription m = gets focusedEntity >>= maybe continueWithoutRedraw descriptionModal
 where
  descriptionModal :: Entity -> EventM Name ScenarioState ()
  descriptionModal e = do
    s <- get
    resetViewport modalScroll
    uiGameplay . uiDialogs . uiModal ?= generateModal m s (MidScenarioModal $ DescriptionModal e)

-- | Attempt to make an entity selected from the inventory, if the
--   base is not currently busy.
makeFocusedEntity :: EventM Name ScenarioState ()
makeFocusedEntity = gets focusedEntity >>= maybe continueWithoutRedraw makeEntity
 where
  makeEntity :: Entity -> EventM Name ScenarioState ()
  makeEntity e = do
    s <- get
    let name = e ^. entityName
        mkT = [tmQ| make $str:name |]
    case isActive <$> (s ^? gameState . baseRobot) of
      Just False -> runBaseTerm (Just mkT)
      _ -> continueWithoutRedraw

showZero :: EventM Name UIInventory ()
showZero = uiShowZero %= not

cycleSort :: EventM Name UIInventory ()
cycleSort = uiInventorySort %= cycleSortOrder

switchSortDirection :: EventM Name UIInventory ()
switchSortDirection = uiInventorySort %= cycleSortDirection

searchInventory :: EventM Name UIInventory ()
searchInventory = uiInventorySearch .= Just ""

-- | Handle an event to navigate through the inventory list.
handleInventoryListEvent :: V.Event -> EventM Name UIGameplay ()
handleInventoryListEvent ev = do
  -- Note, refactoring like this is tempting:
  --
  --   Brick.zoom (uiState . ... . _Just . _2) (handleListEventWithSeparators ev (is _Separator))
  --
  -- However, this does not work since we want to skip redrawing in the no-list case!
  mList <- preuse $ uiInventory . uiInventoryList . _Just . _2
  case mList of
    Nothing -> continueWithoutRedraw
    Just l -> do
      when (isValidListMovement ev) $ resetViewport infoScroll
      l' <- nestEventM' l (handleListEventWithSeparators ev (is _Separator))
      uiInventory . uiInventoryList . _Just . _2 .= l'

-- ----------------------------------------------
--               INVENTORY SEARCH
-- ----------------------------------------------

-- | Handle a user input event in the robot/inventory panel, while in
--   inventory search mode.
handleInventorySearchEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleInventorySearchEvent = \case
  -- Escape: stop filtering and go back to regular inventory mode
  EscapeKey ->
    zoomInventory $ uiInventorySearch .= Nothing
  -- Enter: return to regular inventory mode, and pop out the selected item
  Key V.KEnter -> do
    zoomInventory $ uiInventorySearch .= Nothing
    scenarioStateWithMenu showEntityDescription
  -- Any old character: append to the current search string
  CharKey c -> do
    resetViewport infoScroll
    zoomInventory $ uiInventorySearch %= fmap (`snoc` c)
  -- Backspace: chop the last character off the end of the current search string
  BackspaceKey -> do
    zoomInventory $ uiInventorySearch %= fmap (T.dropEnd 1)
  -- Handle any other event as list navigation, so we can look through
  -- the filtered inventory using e.g. arrow keys
  VtyEvent ev -> Brick.zoom (playState . scenarioState . uiGameplay) $ handleInventoryListEvent ev
  _ -> continueWithoutRedraw

-- ----------------------------------------------
--                 HELPER UTILS
-- ----------------------------------------------

zoomInventory :: EventM Name UIInventory () -> EventM Name AppState ()
zoomInventory act = Brick.zoom (playState . scenarioState . uiGameplay . uiInventory) $ do
  uiInventoryShouldUpdate .= True
  act
