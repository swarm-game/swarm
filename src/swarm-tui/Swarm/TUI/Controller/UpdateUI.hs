{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The main TUI update logic that is called from other controller parts.
module Swarm.TUI.Controller.UpdateUI (
  updateUI,
  updateAndRedrawUI,
  updateRobotDetailsPane,
) where

-- See Note [liftA2 re-export from Prelude]

import Brick hiding (Direction, Location, on)
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens as Lens
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List.Extra (enumerate)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text qualified as T
import Data.Vector qualified as V
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Popup (Popup (..), addPopup)
import Swarm.Game.Robot
import Swarm.Game.Robot.Activity
import Swarm.Game.Robot.Concrete
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Substate
import Swarm.Game.Tick (TickNumber)
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Types
import Swarm.Language.Value (Value (VExc, VUnit), emptyEnv, envTydefs, prettyValue)
import Swarm.Pretty
import Swarm.TUI.Controller.SaveScenario (saveScenarioInfoOnFinishNocheat)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (..))
import Swarm.TUI.Model.Dialog.Goal
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.Model.ViewChunk
import Swarm.TUI.View.Objective qualified as GR
import Swarm.TUI.View.Robot
import Swarm.TUI.View.Robot.Type
import System.Clock (getTime, Clock (Monotonic))
import Witch (into)

-- | Update the UI and redraw if needed.
--
-- This function is used after running the game for some number of ticks.
updateAndRedrawUI :: Bool -> EventM Name AppState ()
updateAndRedrawUI forceRedraw = do
  shouldRedraw <- updateUI
  unless (forceRedraw || shouldRedraw) continueWithoutRedraw

checkInventoryUpdated :: Maybe Robot -> EventM Name ScenarioState Bool
checkInventoryUpdated fr = do
  -- The hash of the robot whose inventory is currently displayed (if any)
  listRobotHash <- fmap fst <$> use (uiGameplay . uiInventory . uiInventoryList)

  -- The hash of the focused robot (if any)
  let focusedRobotHash = view inventoryHash <$> fr

  -- Check if the inventory list needs to be updated.
  shouldUpdate <- use (uiGameplay . uiInventory . uiInventoryShouldUpdate)

  -- Whether the focused robot is too far away to sense, & whether
  -- that has recently changed
  dist <- use (gameState . to focusedRange)
  farOK <- liftA2 (||) (use (gameState . creativeMode)) (use (gameState . landscape . worldScrollable))
  let tooFar = not farOK && dist == Just Far
      farChanged = tooFar /= isNothing listRobotHash

  -- If the robot moved in or out of range, or hashes don't match
  -- (either because which robot (or whether any robot) is focused
  -- changed, or the focused robot's inventory changed), or the
  -- inventory was flagged to be updated, regenerate the inventory list.
  let shouldRegenerateInventory = farChanged || listRobotHash /= focusedRobotHash || shouldUpdate
  when shouldRegenerateInventory $
    Brick.zoom (uiGameplay . uiInventory) $ do
      populateInventoryList $ if tooFar then Nothing else fr
      uiInventoryShouldUpdate .= False
  return shouldRegenerateInventory

checkReplUpdated :: GameState -> EventM Name ScenarioState Bool
checkReplUpdated g = case g ^. gameControls . replStatus of
  -- Now check if the base finished running a program entered at the REPL.
  REPLWorking pty (Just v)
    -- It did, and the result was the unit value or an exception.  Just reset replStatus.
    | v `elem` [VUnit, VExc] -> do
        listener <- use $ gameState . gameControls . replListener
        liftIO $ listener ""
        gameState . gameControls . replStatus .= REPLDone (Just (pty, v))
        pure True

    -- It did, and returned some other value.  Create new 'it'
    -- variables, pretty-print the result as a REPL output, with its
    -- type, and reset the replStatus.
    | otherwise -> do
        itIx <- use (gameState . gameControls . replNextValueIndex)
        env <- fromMaybe emptyEnv <$> preuse (gameState . baseEnv)
        let finalType = stripCmd (env ^. envTydefs) pty
            itName = fromString $ "it" ++ show itIx
            out = T.intercalate " " [itName, ":", prettyText finalType, "=", into (prettyValue v)]
        addREPLHistItem REPLOutput out
        listener <- use $ gameState . gameControls . replListener
        liftIO $ listener out
        invalidateCacheEntry REPLHistoryCache
        vScrollToEnd replScroll

        Brick.zoom gameState $ do
          gameControls . replStatus .= REPLDone (Just (finalType, v))
          baseEnv . at itName .= Just (Typed v finalType mempty)
          baseEnv . at "it" .= Just (Typed v finalType mempty)
          gameControls . replNextValueIndex %= (+ 1)
        pure True

  -- Otherwise, do nothing.
  _ -> pure False

checkLogUpdated :: Maybe Robot -> EventM Name ScenarioState Bool
checkLogUpdated fr = do
  -- If the inventory or info panels are currently focused, it would
  -- be rude to update them right under the user's nose, so consider
  -- them "sticky".  They will be updated as soon as the player moves
  -- the focus away.
  fring <- use $ uiGameplay . uiFocusRing
  let sticky = focusGetCurrent fring `elem` map (Just . FocusablePanel) [RobotPanel, InfoPanel]

  -- Check if the robot log was updated and we are allowed to change
  -- the inventory+info panels.
  case maybe False (view robotLogUpdated) fr && not sticky of
    False -> pure False
    True -> do
      -- Reset the log updated flag
      zoomGameStateFromScenarioState $ zoomRobots clearFocusedRobotLogUpdated

      -- Find and focus an equipped "logger" device in the inventory list.
      let isLogger (EquippedEntry e) = e ^. entityName == "logger"
          isLogger _ = False
          focusLogger = BL.listFindBy isLogger

      uiGameplay . uiInventory . uiInventoryList . _Just . _2 %= focusLogger

      -- Now inform the UI that it should scroll the info panel to
      -- the very end.
      uiGameplay . uiScrollToEnd .= True
      pure True

-- | Check whether we're currently hiding all robots and it's time to
--   unhide them.
checkUnhideRobots :: EventM Name ScenarioState ()
checkUnhideRobots = do
  t <- liftIO $ getTime Monotonic
  hideTime <- use (uiGameplay . uiHideRobotsUntil)
  case hideTime of
    Nothing -> pure ()
    Just ht -> when (t >= ht) $ do
      uiGameplay . uiHideRobotsUntil .= Nothing
      Brick.zoom (gameState . redraw) $ redrawWorld .= True

-- | Update the UI.  This function is used after running the
--   game for some number of ticks.
updateUI :: EventM Name AppState Bool
updateUI = do
  Brick.zoom (playState . scenarioState) checkUnhideRobots
  Brick.zoom (playState . scenarioState . gameState) loadVisibleRegion

  g <- use $ playState . scenarioState . gameState

  -- Some part of the world needs a redraw if either needsRedraw is
  -- set (meaning the world must be completely redrawn), or there are
  -- some cells marked as dirty
  let worldPanelUpdated = needsRedraw (g ^. redraw)

  if (g ^. redraw . redrawWorld)
    then invalidateCache   -- Invalidate entire view chunk cache
    else
      -- Invalidate cache entries for view chunks containing cells that were updated,
      -- so they will be redrawn.
      forM_ (g ^. redraw . dirtyCells) $ invalidateCacheEntry . ViewChunkCache . viewChunkFor

  -- Reset the redraw state.
  playState . scenarioState . gameState . redraw %= resetRedraw

  let fr = g ^. to focusedRobot
  inventoryUpdated <- Brick.zoom (playState . scenarioState) $ checkInventoryUpdated fr

  replUpdated <- Brick.zoom (playState . scenarioState) $ do
    -- NORMAL REPL UPDATE ---
    -- Now check if the base finished running a program entered at the REPL.
    b <- checkReplUpdated g
    -- REPL REPLAY ----------
    replayRepl
    pure b

  -- If the focused robot's log has been updated and the UI focus
  -- isn't currently on the inventory or info panels, attempt to
  -- automatically switch to the logger and scroll all the way down so
  -- the new message can be seen.
  playState . scenarioState . uiGameplay . uiScrollToEnd .= False
  logUpdated <- Brick.zoom (playState . scenarioState) $ checkLogUpdated fr

  menu <- use $ uiState . uiMenu
  dOps <- use $ uiState . uiDebugOptions
  goalOrWinUpdated <- Brick.zoom playState $ doGoalUpdates dOps menu

  newPopups <- Brick.zoom playState generateNotificationPopups

  -- Update the robots modal only when it is enabled.  See #2370.
  curModal <- use $ playState . scenarioState . uiGameplay . uiDialogs . uiModal
  when ((view modalType <$> curModal) == Just (MidScenarioModal RobotsModal)) $
    Brick.zoom (playState . scenarioState . uiGameplay . uiDialogs . uiRobot) $
      doRobotListUpdate dOps g

  let shouldRedraw =
        worldPanelUpdated
          || inventoryUpdated
          || replUpdated
          || logUpdated
          || goalOrWinUpdated
          || newPopups
  pure shouldRedraw

replayRepl :: EventM Name ScenarioState ()
replayRepl = do
  replControl <- use $ uiGameplay . uiREPL . replControlMode
  when (replControl == Replaying) $ do
    tick <- use $ gameState . temporal . ticks
    replay <- use $ uiGameplay . uiREPLReplay
    case dropWhile notInput replay of
      (item : rest)
        -- replay current repl item
        | item.replItemTick <= tick -> do
            uiGameplay . uiREPLReplay .= rest
            inProgress <- use $ gameState . gameControls . replWorking
            if inProgress then handleStillRunning item tick else void $ runBaseCode item.replItemText
            invalidateCacheEntry REPLHistoryCache
            vScrollToEnd replScroll
        -- not yet time of repl item, lets at least save the dropWhile
        | otherwise -> uiGameplay . uiREPLReplay .= (item : rest)
      -- nothing left to replay
      [] -> exitReplayMode
 where
  exitReplayMode :: EventM Name ScenarioState ()
  exitReplayMode = do
    uiGameplay . uiREPLReplay .= []
    uiGameplay . uiREPL . replControlMode .= Typing
  handleStillRunning :: REPLHistItem -> TickNumber -> EventM Name ScenarioState ()
  handleStillRunning item tick = do
    addREPLHistItem REPLError . T.pack $
      "Can not replay REPL item, base is still running."
        <> " ITEM: "
        <> show item
        <> " CURRENT: "
        <> show tick
    exitReplayMode
  notInput :: REPLHistItem -> Bool
  notInput e = replItemType e /= REPLEntry Submitted

doRobotListUpdate :: Set DebugOption -> GameState -> EventM Name RobotDisplay ()
doRobotListUpdate dOps g = do
  robotsGridList %= updateRobotList dOps g
  rList <- use robotsGridList
  let mRob = getSelectedRobot g rList
  forM_ mRob $ \r -> do
    Brick.zoom robotDetailsPaneState $ updateRobotDetailsPane r

updateRobotDetailsPane :: Robot -> EventM Name RobotDetailsPaneState ()
updateRobotDetailsPane rob = do
  cmdHistogramList . BL.listElementsL .= V.fromList (M.toList (rob ^. activityCounts . commandsHistogram))
  logsList . BL.listElementsL .= (rob ^. robotLog)

-- | Either pops up the updated Goals modal
-- or pops up the Congratulations (Win) modal, or pops
-- up the Condolences (Lose) modal.
-- The Win modal will take precedence if the player
-- has met the necessary conditions to win the game.
--
-- If the player chooses to "Keep Playing" from the Win modal, the
-- updated Goals will then immediately appear.
-- This is desirable for:
-- * feedback as to the final goal the player accomplished,
-- * as a summary of all of the goals of the game
-- * shows the player more "optional" goals they can continue to pursue
doGoalUpdates :: Set DebugOption -> Menu -> EventM Name PlayState Bool
doGoalUpdates dOpts menu = do
  curGoal <- use (scenarioState . uiGameplay . uiDialogs . uiGoal . goalsContent)
  curWinCondition <- use (scenarioState . gameState . winCondition)
  announcementsList <- use (scenarioState . gameState . messageInfo . announcementQueue . to toList)
  let showHiddenGoals = ShowHiddenGoals `S.member` dOpts

  -- Decide whether we need to update the current goal text and pop
  -- up a modal dialog.
  case curWinCondition of
    NoWinCondition -> return False
    WinConditions (Unwinnable False) x ->
      setFinishState LoseModal (Unwinnable True) x
    WinConditions (Won False ts) x -> do
      setFinishState WinModal (Won True ts) x
    WinConditions _ oc -> Brick.zoom scenarioState $ do
      currentModal <- preuse $ uiGameplay . uiDialogs . uiModal . _Just . modalType
      let newGoalTracking = GoalTracking announcementsList $ constructGoalMap showHiddenGoals oc
          -- The "uiGoal" field is initialized with empty members, so we know that
          -- this will be the first time showing it if it will be nonempty after previously
          -- being empty.
          isFirstGoalDisplay = hasAnythingToShow newGoalTracking && not (hasAnythingToShow curGoal)
          goalWasUpdated = isFirstGoalDisplay || not (null announcementsList)
          isEnding = maybe False isEndingModal currentModal

      -- Decide whether to show a pop-up modal congratulating the user on
      -- successfully completing the current challenge.
      when (goalWasUpdated && not isEnding) $ do
        -- The "uiGoal" field is necessary at least to "persist" the data that is needed
        -- if the player chooses to later "recall" the goals dialog with CTRL+g.
        uiGameplay . uiDialogs . uiGoal .= goalDisplay newGoalTracking

        -- This clears the "flag" that indicate that the goals dialog needs to be
        -- automatically popped up.
        gameState . messageInfo . announcementQueue .= mempty

        showObjectives <- use $ uiGameplay . uiAutoShowObjectives
        when showObjectives $ openMidScenarioModal GoalModal

      return goalWasUpdated
 where
  setFinishState :: ScenarioOutcome -> WinStatus -> ObjectiveCompletion -> EventM Name PlayState Bool
  setFinishState m result x = do
    -- This clears the "flag" that the Lose dialog needs to pop up
    scenarioState . gameState . winCondition .= WinConditions result x
    openEndScenarioModal menu $ ScenarioFinishModal m

    saveScenarioInfoOnFinishNocheat dOpts

    -- We do NOT advance the New Game menu to the next item here (we
    -- used to!), because we do not know if the user is going to
    -- select 'keep playing' or 'next challenge'.  We maintain the
    -- invariant that the current menu item is always the same as
    -- the scenario currently being played.  If the user either (1)
    -- quits to the menu or (2) selects 'next challenge' we will
    -- advance the menu at that point.
    return True

  goalDisplay :: GoalTracking -> GoalDisplay
  goalDisplay newGoalTracking =
    let multiple = hasMultipleGoals newGoalTracking
     in GoalDisplay
          newGoalTracking
          (GR.makeListWidget newGoalTracking)
          (focusSetCurrent (GoalWidgets $ goalFocus multiple) (goalFocusRing multiple))

  goalFocus :: Bool -> GoalWidget
  goalFocus hasMultiple = if hasMultiple then ObjectivesList else GoalSummary

  goalFocusRing :: Bool -> FocusRing Name
  goalFocusRing hasMultiple = focusRing $ GoalWidgets <$> if hasMultiple then enumerate else [GoalSummary]

  isEndingModal :: ModalType -> Bool
  isEndingModal = \case
    EndScenarioModal _ -> True
    _ -> False

-- | Pops up notifications when new recipes or commands are unlocked.
generateNotificationPopups :: EventM Name PlayState Bool
generateNotificationPopups = do
  rs <- use $ scenarioState . gameState . discovery . availableRecipes
  let newRecipes = rs ^. notificationsShouldAlert
  when newRecipes $ do
    progression . uiPopups %= addPopup RecipesPopup
    scenarioState . gameState . discovery . availableRecipes . notificationsShouldAlert .= False

  cs <- use $ scenarioState . gameState . discovery . availableCommands
  let alertCommands = cs ^. notificationsShouldAlert
  when alertCommands $ do
    let newCommands = take (cs ^. notificationsCount) (cs ^. notificationsContent)
    progression . uiPopups %= addPopup (CommandsPopup newCommands)
    scenarioState . gameState . discovery . availableCommands . notificationsShouldAlert .= False

  return $ newRecipes || alertCommands

-- | Strips the top-level @Cmd@ from a type, if any (to compute the
--   result type of a REPL command evaluation).
stripCmd :: TDCtx -> Polytype -> Polytype
stripCmd tdCtx = fmap $ \ty -> case whnfType tdCtx ty of
  TyCmd resTy -> resTy
  _ -> ty
