{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The main TUI update logic that is called from other controller parts.
module Swarm.TUI.Controller.UpdateUI (
  updateUI,
  updateAndRedrawUI,
) where

import Brick hiding (Direction, Location)
import Brick.Focus

-- See Note [liftA2 re-export from Prelude]
import Brick.Widgets.List qualified as BL
import Control.Applicative (liftA2, pure)
import Control.Lens as Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List.Extra (enumerate)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text qualified as T
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Substate
import Swarm.Language.Pretty
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Types
import Swarm.Language.Value (Value (VExc, VUnit), envTydefs, prettyValue)
import Swarm.TUI.Controller.SaveScenario (saveScenarioInfoOnFinishNocheat)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (..))
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Popup (Popup (..), addPopup)
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.View.Objective qualified as GR
import Witch (into)
import Prelude hiding (Applicative (..))

-- | Update the UI and redraw if needed.
--
-- This function is used after running the game for some number of ticks.
updateAndRedrawUI :: Bool -> EventM Name AppState ()
updateAndRedrawUI forceRedraw = do
  redraw <- updateUI
  unless (forceRedraw || redraw) continueWithoutRedraw

-- | Update the UI.  This function is used after running the
--   game for some number of ticks.
updateUI :: EventM Name AppState Bool
updateUI = do
  loadVisibleRegion

  -- If the game state indicates a redraw is needed, invalidate the
  -- world cache so it will be redrawn.
  g <- use gameState
  when (g ^. needsRedraw) $ invalidateCacheEntry WorldCache

  -- The hash of the robot whose inventory is currently displayed (if any)
  listRobotHash <- fmap fst <$> use (uiState . uiGameplay . uiInventory . uiInventoryList)

  -- The hash of the focused robot (if any)
  fr <- use (gameState . to focusedRobot)
  let focusedRobotHash = view inventoryHash <$> fr

  -- Check if the inventory list needs to be updated.
  shouldUpdate <- use (uiState . uiGameplay . uiInventory . uiInventoryShouldUpdate)

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
  inventoryUpdated <-
    if farChanged || (not farChanged && listRobotHash /= focusedRobotHash) || shouldUpdate
      then do
        Brick.zoom (uiState . uiGameplay . uiInventory) $ do
          populateInventoryList $ if tooFar then Nothing else fr
          uiInventoryShouldUpdate .= False
        pure True
      else pure False

  -- Now check if the base finished running a program entered at the REPL.
  replUpdated <- case g ^. gameControls . replStatus of
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
          gameState . gameControls . replStatus .= REPLDone (Just (finalType, v))
          gameState . baseEnv . at itName .= Just (Typed v finalType mempty)
          gameState . baseEnv . at "it" .= Just (Typed v finalType mempty)
          gameState . gameControls . replNextValueIndex %= (+ 1)
          pure True

    -- Otherwise, do nothing.
    _ -> pure False

  -- If the focused robot's log has been updated and the UI focus
  -- isn't currently on the inventory or info panels, attempt to
  -- automatically switch to the logger and scroll all the way down so
  -- the new message can be seen.
  uiState . uiGameplay . uiScrollToEnd .= False
  logUpdated <- do
    -- If the inventory or info panels are currently focused, it would
    -- be rude to update them right under the user's nose, so consider
    -- them "sticky".  They will be updated as soon as the player moves
    -- the focus away.
    fring <- use $ uiState . uiGameplay . uiFocusRing
    let sticky = focusGetCurrent fring `elem` map (Just . FocusablePanel) [RobotPanel, InfoPanel]

    -- Check if the robot log was updated and we are allowed to change
    -- the inventory+info panels.
    case maybe False (view robotLogUpdated) fr && not sticky of
      False -> pure False
      True -> do
        -- Reset the log updated flag
        zoomGameState $ zoomRobots clearFocusedRobotLogUpdated

        -- Find and focus an equipped "logger" device in the inventory list.
        let isLogger (EquippedEntry e) = e ^. entityName == "logger"
            isLogger _ = False
            focusLogger = BL.listFindBy isLogger

        uiState . uiGameplay . uiInventory . uiInventoryList . _Just . _2 %= focusLogger

        -- Now inform the UI that it should scroll the info panel to
        -- the very end.
        uiState . uiGameplay . uiScrollToEnd .= True
        pure True

  goalOrWinUpdated <- doGoalUpdates

  newPopups <- generateNotificationPopups

  let redraw =
        g ^. needsRedraw
          || inventoryUpdated
          || replUpdated
          || logUpdated
          || goalOrWinUpdated
          || newPopups
  pure redraw

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
doGoalUpdates :: EventM Name AppState Bool
doGoalUpdates = do
  curGoal <- use (uiState . uiGameplay . uiGoal . goalsContent)
  curWinCondition <- use (gameState . winCondition)
  announcementsSeq <- use (gameState . messageInfo . announcementQueue)
  let announcementsList = toList announcementsSeq

  -- Decide whether we need to update the current goal text and pop
  -- up a modal dialog.
  case curWinCondition of
    NoWinCondition -> return False
    WinConditions (Unwinnable False) x -> do
      -- This clears the "flag" that the Lose dialog needs to pop up
      gameState . winCondition .= WinConditions (Unwinnable True) x
      openModal $ ScenarioEndModal LoseModal
      saveScenarioInfoOnFinishNocheat
      return True
    WinConditions (Won False ts) x -> do
      -- This clears the "flag" that the Win dialog needs to pop up
      gameState . winCondition .= WinConditions (Won True ts) x
      openModal $ ScenarioEndModal WinModal
      saveScenarioInfoOnFinishNocheat
      -- We do NOT advance the New Game menu to the next item here (we
      -- used to!), because we do not know if the user is going to
      -- select 'keep playing' or 'next challenge'.  We maintain the
      -- invariant that the current menu item is always the same as
      -- the scenario currently being played.  If the user either (1)
      -- quits to the menu or (2) selects 'next challenge' we will
      -- advance the menu at that point.
      return True
    WinConditions _ oc -> do
      showHiddenGoals <- use $ uiState . uiDebugOptions . Lens.contains ShowHiddenGoals
      let newGoalTracking = GoalTracking announcementsList $ constructGoalMap showHiddenGoals oc
          -- The "uiGoal" field is initialized with empty members, so we know that
          -- this will be the first time showing it if it will be nonempty after previously
          -- being empty.
          isFirstGoalDisplay = hasAnythingToShow newGoalTracking && not (hasAnythingToShow curGoal)
          goalWasUpdated = isFirstGoalDisplay || not (null announcementsList)

      -- Decide whether to show a pop-up modal congratulating the user on
      -- successfully completing the current challenge.
      when goalWasUpdated $ do
        let hasMultiple = hasMultipleGoals newGoalTracking
            defaultFocus =
              if hasMultiple
                then ObjectivesList
                else GoalSummary

            ring =
              focusRing $
                map GoalWidgets $
                  if hasMultiple
                    then enumerate
                    else [GoalSummary]

        -- The "uiGoal" field is necessary at least to "persist" the data that is needed
        -- if the player chooses to later "recall" the goals dialog with CTRL+g.
        uiState
          . uiGameplay
          . uiGoal
          .= GoalDisplay
            newGoalTracking
            (GR.makeListWidget newGoalTracking)
            (focusSetCurrent (GoalWidgets defaultFocus) ring)

        -- This clears the "flag" that indicate that the goals dialog needs to be
        -- automatically popped up.
        gameState . messageInfo . announcementQueue .= mempty

        isAutoPlay <- use $ uiState . uiGameplay . uiIsAutoPlay
        showGoalsAnyway <- use $ uiState . uiDebugOptions . Lens.contains ShowGoalDialogsInAutoPlay
        unless (isAutoPlay && not showGoalsAnyway) $
          openModal GoalModal

      return goalWasUpdated

-- | Pops up notifications when new recipes or commands are unlocked.
generateNotificationPopups :: EventM Name AppState Bool
generateNotificationPopups = do
  rs <- use $ gameState . discovery . availableRecipes
  let newRecipes = rs ^. notificationsShouldAlert
  when newRecipes $ do
    uiState . uiPopups %= addPopup RecipesPopup
    gameState . discovery . availableRecipes . notificationsShouldAlert .= False

  cs <- use $ gameState . discovery . availableCommands
  let alertCommands = cs ^. notificationsShouldAlert
  when alertCommands $ do
    let newCommands = take (cs ^. notificationsCount) (cs ^. notificationsContent)
    uiState . uiPopups %= addPopup (CommandsPopup newCommands)
    gameState . discovery . availableCommands . notificationsShouldAlert .= False

  return $ newRecipes || alertCommands

-- | Strips the top-level @Cmd@ from a type, if any (to compute the
--   result type of a REPL command evaluation).
stripCmd :: TDCtx -> Polytype -> Polytype
stripCmd tdCtx (Forall xs ty) = case whnfType tdCtx ty of
  TyCmd resTy -> Forall xs resTy
  _ -> Forall xs ty
