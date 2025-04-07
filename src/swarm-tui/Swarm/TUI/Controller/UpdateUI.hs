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
import Control.Applicative (liftA2, pure)
import Control.Lens as Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List.Extra (enumerate)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set (Set)
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
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Types
import Swarm.Language.Value (Value (VExc, VUnit), envTydefs, prettyValue)
import Swarm.Pretty
import Swarm.TUI.Controller.SaveScenario (saveScenarioInfoOnFinishNocheat)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (..))
import Swarm.TUI.Model.Dialog.Goal
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Objective qualified as GR
import Swarm.TUI.View.Robot
import Swarm.TUI.View.Robot.Type
import Swarm.Util (applyJust)
import Witch (into)
import Prelude hiding (Applicative (..))

-- | Update the UI and redraw if needed.
--
-- This function is used after running the game for some number of ticks.
updateAndRedrawUI :: Bool -> EventM Name AppState ()
updateAndRedrawUI forceRedraw = do
  redraw <- updateUI
  unless (forceRedraw || redraw) continueWithoutRedraw

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
        env <- use (gameState . baseEnv)
        let finalType = stripCmd (env ^. envTydefs) pty
            itName = fromString $ "it" ++ show itIx
            out = T.intercalate " " [itName, ":", prettyText finalType, "=", into (prettyValue v)]
        addREPLHistItem (mkREPLOutput out)
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

-- | Update the UI.  This function is used after running the
--   game for some number of ticks.
updateUI :: EventM Name AppState Bool
updateUI = do
  g <- use $ playState . scenarioState . gameState

  Brick.zoom (playState . scenarioState . gameState) loadVisibleRegion

  -- If the game state indicates a redraw is needed, invalidate the
  -- world cache so it will be redrawn.
  when (g ^. needsRedraw) $ invalidateCacheEntry WorldCache

  let fr = g ^. to focusedRobot
  inventoryUpdated <- Brick.zoom (playState . scenarioState) $ checkInventoryUpdated fr

  -- Now check if the base finished running a program entered at the REPL.
  replUpdated <- Brick.zoom (playState . scenarioState) $ checkReplUpdated g

  -- If the focused robot's log has been updated and the UI focus
  -- isn't currently on the inventory or info panels, attempt to
  -- automatically switch to the logger and scroll all the way down so
  -- the new message can be seen.
  playState . scenarioState . uiGameplay . uiScrollToEnd .= False
  logUpdated <- Brick.zoom (playState . scenarioState) $ checkLogUpdated fr

  menu <- use $ uiState . uiMenu
  goalOrWinUpdated <- doGoalUpdates menu

  newPopups <- generateNotificationPopups

  -- Update the robots modal only when it is enabled.  See #2370.
  curModal <- use $ playState . scenarioState . uiGameplay . uiDialogs . uiModal
  when ((view modalType <$> curModal) == Just RobotsModal) $ do
    dOps <- use $ uiState . uiDebugOptions
    Brick.zoom (playState . scenarioState) $ doRobotListUpdate dOps g

  let redraw =
        g ^. needsRedraw
          || inventoryUpdated
          || replUpdated
          || logUpdated
          || goalOrWinUpdated
          || newPopups
  pure redraw

doRobotListUpdate :: Set DebugOption -> GameState -> EventM Name ScenarioState ()
doRobotListUpdate dOps g = do
  gp <- use uiGameplay

  let rd =
        mkRobotDisplay $
          RobotRenderingContext
            { _mygs = g
            , _gameplay = gp
            , _timing = gp ^. uiTiming
            , _uiDbg = dOps
            }
      oldList = getList $ gp ^. uiDialogs . uiRobot . robotListContent . robotsListWidget
      maybeOldSelected = snd <$> BL.listSelectedElement oldList

      -- Since we're replacing the entire contents of the list, we need to preserve the
      -- selected row here.
      maybeModificationFunc =
        updateList . BL.listFindBy . ((==) `on` view (robot . robotID)) <$> maybeOldSelected

  uiGameplay . uiDialogs . uiRobot . robotListContent . robotsListWidget .= applyJust maybeModificationFunc rd

  Brick.zoom (uiGameplay . uiDialogs . uiRobot) $
    forM_ maybeOldSelected updateRobotDetailsPane

updateRobotDetailsPane :: RobotWidgetRow -> EventM Name RobotDisplay ()
updateRobotDetailsPane robotPayload =
  Brick.zoom robotListContent $ do
    robotDetailsPaneState . cmdHistogramList . BL.listElementsL .= V.fromList (M.toList (robotPayload ^. robot . activityCounts . commandsHistogram))
    robotDetailsPaneState . logsList . BL.listElementsL .= robotPayload ^. robot . robotLog

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
doGoalUpdates :: Menu -> EventM Name AppState Bool
doGoalUpdates menu = do
  curGoal <- use (playState . scenarioState . uiGameplay . uiDialogs . uiGoal . goalsContent)
  curWinCondition <- use (playState . scenarioState . gameState . winCondition)
  announcementsList <- use (playState . scenarioState . gameState . messageInfo . announcementQueue . to toList)
  showHiddenGoals <- use $ uiState . uiDebugOptions . Lens.contains ShowHiddenGoals

  -- Decide whether we need to update the current goal text and pop
  -- up a modal dialog.
  case curWinCondition of
    NoWinCondition -> return False
    WinConditions (Unwinnable False) x -> do
      Brick.zoom (playState . scenarioState) $ do
        -- This clears the "flag" that the Lose dialog needs to pop up
        gameState . winCondition .= WinConditions (Unwinnable True) x
        openModal menu $ ScenarioEndModal LoseModal
      saveScenarioInfoOnFinishNocheat
      return True
    WinConditions (Won False ts) x -> do
      Brick.zoom (playState . scenarioState) $ do
        -- This clears the "flag" that the Win dialog needs to pop up
        gameState . winCondition .= WinConditions (Won True ts) x
        openModal menu $ ScenarioEndModal WinModal
      saveScenarioInfoOnFinishNocheat
      -- We do NOT advance the New Game menu to the next item here (we
      -- used to!), because we do not know if the user is going to
      -- select 'keep playing' or 'next challenge'.  We maintain the
      -- invariant that the current menu item is always the same as
      -- the scenario currently being played.  If the user either (1)
      -- quits to the menu or (2) selects 'next challenge' we will
      -- advance the menu at that point.
      return True
    WinConditions _ oc -> Brick.zoom (playState . scenarioState) $ do
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
        when showObjectives $ openModal menu GoalModal

      return goalWasUpdated
 where
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
    ScenarioEndModal {} -> True
    QuitModal -> True
    KeepPlayingModal -> True
    _ -> False

-- | Pops up notifications when new recipes or commands are unlocked.
generateNotificationPopups :: EventM Name AppState Bool
generateNotificationPopups = do
  rs <- use $ playState . scenarioState . gameState . discovery . availableRecipes
  let newRecipes = rs ^. notificationsShouldAlert
  when newRecipes $ do
    runtimeState . progression . uiPopups %= addPopup RecipesPopup
    playState . scenarioState . gameState . discovery . availableRecipes . notificationsShouldAlert .= False

  cs <- use $ playState . scenarioState . gameState . discovery . availableCommands
  let alertCommands = cs ^. notificationsShouldAlert
  when alertCommands $ do
    let newCommands = take (cs ^. notificationsCount) (cs ^. notificationsContent)
    runtimeState . progression . uiPopups %= addPopup (CommandsPopup newCommands)
    playState . scenarioState . gameState . discovery . availableCommands . notificationsShouldAlert .= False

  return $ newRecipes || alertCommands

-- | Strips the top-level @Cmd@ from a type, if any (to compute the
--   result type of a REPL command evaluation).
stripCmd :: TDCtx -> Polytype -> Polytype
stripCmd tdCtx = fmap $ \ty -> case whnfType tdCtx ty of
  TyCmd resTy -> resTy
  _ -> ty
