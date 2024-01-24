{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Stepping robot CESK machines
--
-- Facilities for stepping the robot CESK machines, /i.e./ the actual
-- interpreter for the Swarm language.
--
-- == Note on the IO:
--
-- The only reason we need @IO@ is so that robots can run programs
-- loaded from files, via the 'Swarm.Language.Syntax.Run' command.
-- This could be avoided by using a hypothetical @import@ command instead and parsing
-- the required files at the time of declaration.
-- See <https://github.com/swarm-game/swarm/issues/495>.
module Swarm.Game.Step where

import Control.Applicative (Applicative (..))
import Control.Carrier.Error.Either (ErrorC, runError)
import Control.Carrier.State.Lazy
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Lens as Lens hiding (Const, distrib, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Control.Monad (foldM, forM_, unless, when)
import Data.Bool (bool)
import Data.Functor (void)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence ((><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Linear (zero)
import Prettyprinter (pretty)
import Swarm.Effect as Effect (Time, getNow)
import Swarm.Game.Achievement.Definitions
import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Exception
import Swarm.Game.Robot
import Swarm.Game.Robot.Activity
import Swarm.Game.Robot.Concrete
import Swarm.Game.Robot.Context
import Swarm.Game.Scenario.Objective qualified as OB
import Swarm.Game.Scenario.Objective.WinCheck qualified as WC
import Swarm.Game.State
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step.Const
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util
import Swarm.Game.Step.Util.Command
import Swarm.Game.Tick
import Swarm.Game.Universe
import Swarm.Language.Capability
import Swarm.Language.Context hiding (delete)
import Swarm.Language.Pipeline
import Swarm.Language.Pretty (BulletList (BulletList, bulletListItems), prettyText)
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Value
import Swarm.Log
import Swarm.Util hiding (both)
import Swarm.Util.WindowedCounter qualified as WC
import System.Clock (TimeSpec)
import Witch (From (from))
import Prelude hiding (Applicative (..), lookup)

-- | The main function to do one game tick.
--
--   Note that the game may be in 'RobotStep' mode and not finish
--   the tick. Use the return value to check whether a full tick happened.
gameTick :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => m Bool
gameTick = do
  time <- use $ temporal . ticks
  zoomRobots $ wakeUpRobotsDoneSleeping time
  active <- use $ robotInfo . activeRobots
  focusedRob <- use $ robotInfo . focusedRobotID

  ticked <-
    use (temporal . gameStep) >>= \case
      WorldTick -> do
        runRobotIDs active
        temporal . ticks %= addTicks 1
        pure True
      RobotStep ss -> singleStep ss focusedRob active

  -- See if the base is finished with a computation, and if so, record
  -- the result in the game state so it can be displayed by the REPL;
  -- also save the current store into the robotContext so we can
  -- restore it the next time we start a computation.
  mr <- use (robotInfo . robotMap . at 0)
  case mr of
    Just r -> do
      res <- use $ gameControls . replStatus
      case res of
        REPLWorking (Typed Nothing ty req) -> case getResult r of
          Just (v, s) -> do
            gameControls . replStatus .= REPLWorking (Typed (Just v) ty req)
            baseRobot . robotContext . defStore .= s
          Nothing -> pure ()
        _otherREPLStatus -> pure ()
    Nothing -> pure ()

  -- Possibly update the view center.
  modify recalcViewCenterAndRedraw

  when ticked $ do
    -- On new tick see if the winning condition for the current objective is met.
    wc <- use winCondition
    case wc of
      WinConditions winState oc -> do
        g <- get @GameState
        em <- use $ landscape . entityMap
        hypotheticalWinCheck em g winState oc
      _ -> pure ()
  return ticked

-- | Finish a game tick in progress and set the game to 'WorldTick' mode afterwards.
--
-- Use this function if you need to unpause the game.
finishGameTick :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => m ()
finishGameTick =
  use (temporal . gameStep) >>= \case
    WorldTick -> pure ()
    RobotStep SBefore -> temporal . gameStep .= WorldTick
    RobotStep _ -> void gameTick >> finishGameTick

-- Insert the robot back to robot map.
-- Will selfdestruct or put the robot to sleep if it has that set.
insertBackRobot :: Has (State GameState) sig m => RID -> Robot -> m ()
insertBackRobot rn rob = do
  time <- use $ temporal . ticks
  zoomRobots $
    if rob ^. selfDestruct
      then deleteRobot rn
      else do
        robotMap %= IM.insert rn rob
        case waitingUntil rob of
          Just wakeUpTime
            -- if w=2 t=1 then we do not needlessly put robot to waiting queue
            | wakeUpTime <= addTicks 2 time -> return ()
            | otherwise -> sleepUntil rn wakeUpTime
          Nothing ->
            unless (isActive rob) (sleepForever rn)

-- Run a set of robots - this is used to run robots before/after the focused one.
runRobotIDs :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => IS.IntSet -> m ()
runRobotIDs robotNames = forM_ (IS.toList robotNames) $ \rn -> do
  mr <- uses (robotInfo . robotMap) (IM.lookup rn)
  forM_ mr (stepOneRobot rn)
 where
  stepOneRobot rn rob = tickRobot rob >>= insertBackRobot rn

-- This is a helper function to do one robot step or run robots before/after.
singleStep :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => SingleStep -> RID -> IS.IntSet -> m Bool
singleStep ss focRID robotSet = do
  let (preFoc, focusedActive, postFoc) = IS.splitMember focRID robotSet
  case ss of
    ----------------------------------------------------------------------------
    -- run robots from the beginning until focused robot
    SBefore -> do
      runRobotIDs preFoc
      temporal . gameStep .= RobotStep (SSingle focRID)
      -- also set ticks of focused robot
      steps <- use $ temporal . robotStepsPerTick
      robotInfo . robotMap . ix focRID . activityCounts . tickStepBudget .= steps
      -- continue to focused robot if there were no previous robots
      -- DO NOT SKIP THE ROBOT SETUP above
      if IS.null preFoc
        then singleStep (SSingle focRID) focRID robotSet
        else return False
    ----------------------------------------------------------------------------
    -- run single step of the focused robot (may skip if inactive)
    SSingle rid | not focusedActive -> do
      singleStep (SAfter rid) rid postFoc -- skip inactive focused robot
    SSingle rid -> do
      mOldR <- uses (robotInfo . robotMap) (IM.lookup focRID)
      case mOldR of
        Nothing | rid == focRID -> do
          debugLog "The debugged robot does not exist! Exiting single step mode."
          runRobotIDs postFoc
          temporal . gameStep .= WorldTick
          temporal . ticks %= addTicks 1
          return True
        Nothing | otherwise -> do
          debugLog "The previously debugged robot does not exist!"
          singleStep SBefore focRID postFoc
        Just oldR -> do
          -- if focus changed we need to finish the previous robot
          newR <- (if rid == focRID then stepRobot else tickRobotRec) oldR
          insertBackRobot focRID newR
          if rid == focRID
            then do
              when (newR ^. activityCounts . tickStepBudget == 0) $
                temporal . gameStep .= RobotStep (SAfter focRID)
              return False
            else do
              -- continue to newly focused
              singleStep SBefore focRID postFoc
    ----------------------------------------------------------------------------
    -- run robots after the focused robot
    SAfter rid | focRID <= rid -> do
      -- This state takes care of two possibilities:
      -- 1. normal - rid == focRID and we finish the tick
      -- 2. changed focus and the newly focused robot has previously run
      --    so we just finish the tick the same way
      runRobotIDs postFoc
      temporal . gameStep .= RobotStep SBefore
      temporal . ticks %= addTicks 1
      return True
    SAfter rid | otherwise -> do
      -- go to single step if new robot is focused
      let (_pre, postRID) = IS.split rid robotSet
      singleStep SBefore focRID postRID
 where
  h = hypotheticalRobot (Out VUnit emptyStore []) 0
  debugLog txt = do
    m <- evalState @Robot h $ createLogEntry RobotError Debug txt
    emitMessage m

-- | An accumulator for folding over the incomplete
-- objectives to evaluate for their completion
data CompletionsWithExceptions = CompletionsWithExceptions
  { exceptions :: [Text]
  , completions :: ObjectiveCompletion
  , completionAnnouncementQueue :: [OB.Objective]
  -- ^ Upon completion, an objective is enqueued.
  -- It is dequeued when displayed on the UI.
  }

-- | Execute the win condition check *hypothetically*: i.e. in a
-- fresh CESK machine, using a copy of the current game state.
--
-- The win check is performed only on "active" goals; that is,
-- the goals that are currently unmet and have had all of their
-- prerequisites satisfied.
-- Note that it may be possible, while traversing through the
-- goal list, for one goal to be met earlier in the list that
-- happens to be a prerequisite later in the traversal. This
-- is why:
-- 1) We must not pre-filter the goals to be traversed based
--    on satisfied prerequisites (i.e. we cannot use the
--    "getActiveObjectives" function).
-- 2) The traversal order must be "reverse topological" order, so
--    that prerequisites are evaluated before dependent goals.
-- 3) The iteration needs to be a "fold", so that state is updated
--    after each element.
hypotheticalWinCheck ::
  (Has (State GameState) sig m, Has Effect.Time sig m, Has (Lift IO) sig m) =>
  EntityMap ->
  GameState ->
  WinStatus ->
  ObjectiveCompletion ->
  m ()
hypotheticalWinCheck em g ws oc = do
  -- We can fully and accurately evaluate the new state of the objectives DAG
  -- in a single pass, so long as we visit it in reverse topological order.
  --
  -- N.B. The "reverse" is essential due to the re-population of the
  -- "incomplete" goal list by cons-ing.
  finalAccumulator <-
    foldM foldFunc initialAccumulator $
      reverse incompleteGoals

  let newWinState = case ws of
        Ongoing -> getNextWinState $ completions finalAccumulator
        _ -> ws

  winCondition .= WinConditions newWinState (completions finalAccumulator)

  case newWinState of
    Unwinnable _ -> do
      grantAchievement LoseScenario
    _ -> return ()

  messageInfo . announcementQueue %= (>< Seq.fromList (map ObjectiveCompleted $ completionAnnouncementQueue finalAccumulator))

  mapM_ handleException $ exceptions finalAccumulator
 where
  getNextWinState completedObjs
    | WC.didWin completedObjs = Won False
    | WC.didLose completedObjs = Unwinnable False
    | otherwise = Ongoing

  (withoutIncomplete, incompleteGoals) = OB.extractIncomplete oc
  initialAccumulator = CompletionsWithExceptions [] withoutIncomplete []

  -- All of the "incomplete" goals have been emptied from the initial accumulator, and
  -- these are what we iterate over with the fold.
  -- Each iteration, we either place the goal back into the "incomplete" bucket, or
  -- we determine that it has been met or impossible and place it into the "completed"
  -- or "unwinnable" bucket, respectively.
  foldFunc (CompletionsWithExceptions exnTexts currentCompletions announcements) obj = do
    v <-
      if WC.isPrereqsSatisfied currentCompletions obj
        then runThrow @Exn . evalState @GameState g $ evalPT $ obj ^. OB.objectiveCondition
        else return $ Right $ VBool False
    let simplified = simplifyResult $ stripVResult <$> v
    return $ case simplified of
      Left exnText ->
        CompletionsWithExceptions
          (exnText : exnTexts)
          -- Push back the incomplete goal that had been popped for inspection
          (OB.addIncomplete obj currentCompletions)
          announcements
      Right boolResult ->
        CompletionsWithExceptions
          exnTexts
          -- Either restore the goal to the incomplete list from which it was popped
          -- or move it to the complete (or unwinnable) bucket.
          (modifyCompletions obj currentCompletions)
          (modifyAnnouncements announcements)
       where
        (modifyCompletions, modifyAnnouncements)
          | boolResult = (OB.addCompleted, (obj :))
          | WC.isUnwinnable currentCompletions obj = (OB.addUnwinnable, id)
          | otherwise = (OB.addIncomplete, id)

  simplifyResult = \case
    Left exn -> Left $ formatExn em exn
    Right (VBool x) -> Right x
    Right val ->
      Left $
        T.unwords
          [ "Non boolean value:"
          , prettyValue val
          , "real:"
          , T.pack (show val)
          ]

  -- Log exceptions in the message queue so we can check for them in tests
  handleException exnText = do
    m <- evalState @Robot h $ createLogEntry RobotError Critical exnText
    emitMessage m
   where
    h = hypotheticalRobot (Out VUnit emptyStore []) 0

evalPT ::
  ( Has Effect.Time sig m
  , Has (Throw Exn) sig m
  , Has (State GameState) sig m
  , Has (Lift IO) sig m
  ) =>
  ProcessedTerm ->
  m Value
evalPT t = evaluateCESK (initMachine t empty emptyStore)

-- | Create a special robot to check some hypothetical, for example the win condition.
--
-- Use ID (-1) so it won't conflict with any robots currently in the robot map.
hypotheticalRobot :: CESK -> TimeSpec -> Robot
hypotheticalRobot m =
  mkRobot
    (-1)
    emptyRobotContext
    emptyActivityCount
    Nothing
    "hypothesis"
    mempty
    defaultCosmicLocation
    zero
    defaultRobotDisplay
    m
    []
    []
    True
    False
    mempty

evaluateCESK ::
  ( Has Effect.Time sig m
  , Has (Throw Exn) sig m
  , Has (State GameState) sig m
  , Has (Lift IO) sig m
  ) =>
  CESK ->
  m Value
evaluateCESK cesk = do
  createdAt <- getNow
  let r = hypotheticalRobot cesk createdAt
  zoomRobots $ addRobot r -- Add the special robot to the robot map, so it can look itself up if needed
  evalState r . runCESK $ cesk

runCESK ::
  ( Has Effect.Time sig m
  , Has (Lift IO) sig m
  , Has (Throw Exn) sig m
  , Has (State GameState) sig m
  , Has (State Robot) sig m
  ) =>
  CESK ->
  m Value
runCESK (Up exn _ []) = throwError exn
runCESK cesk = case finalValue cesk of
  Just (v, _) -> return v
  Nothing -> stepCESK cesk >>= runCESK

------------------------------------------------------------
-- Debugging
------------------------------------------------------------

-- | Print a showable value via the robot's log.
--
-- Useful for debugging.
traceLogShow :: (Has (State GameState) sig m, Has (State Robot) sig m, Show a) => a -> m ()
traceLogShow = void . traceLog Logged Info . from . show

------------------------------------------------------------
-- Stepping robots
------------------------------------------------------------

-- | Run a robot for one tick, which may consist of up to
--   'robotStepsPerTick' CESK machine steps and at most one tangible
--   command execution, whichever comes first.
tickRobot :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => Robot -> m Robot
tickRobot r = do
  steps <- use $ temporal . robotStepsPerTick
  tickRobotRec (r & activityCounts . tickStepBudget .~ steps)

-- | Recursive helper function for 'tickRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
tickRobotRec :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => Robot -> m Robot
tickRobotRec r = do
  time <- use $ temporal . ticks
  case wantsToStep time r && (r ^. runningAtomic || r ^. activityCounts . tickStepBudget > 0) of
    True -> stepRobot r >>= tickRobotRec
    False -> return r

-- | Single-step a robot by decrementing its 'tickStepBudget' counter and
--   running its CESK machine for one step.
stepRobot :: (Has (State GameState) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => Robot -> m Robot
stepRobot r = do
  (r', cesk') <- runState (r & activityCounts . tickStepBudget -~ 1) (stepCESK (r ^. machine))
  -- sendIO $ appendFile "out.txt" (prettyString cesk' ++ "\n")
  t <- use $ temporal . ticks

  isCreative <- use creativeMode
  let shouldTrackActivity = isCreative || not (r' ^. systemRobot)

  return $
    applyWhen shouldTrackActivity (maintainActivityWindow t) $
      r'
        & machine .~ cesk'
        & activityCounts . lifetimeStepCount +~ 1
 where
  maintainActivityWindow t bot =
    bot & (activityCounts . activityWindow %~ WC.insert t)

data SKpair = SKpair Store Cont

-- | Performs some side-effectful computation
-- for an "FImmediate" Frame.
-- Aborts processing the continuation stack
-- if an error is encountered.
--
-- Compare to "withExceptions".
processImmediateFrame ::
  (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) =>
  Value ->
  SKpair ->
  -- | the unreliable computation
  ErrorC Exn m () ->
  m CESK
processImmediateFrame v (SKpair s k) unreliableComputation = do
  wc <- runError unreliableComputation
  case wc of
    Left exn -> return $ Up exn s k
    Right () -> stepCESK $ Out v s k

-- | The main CESK machine workhorse.  Given a robot, look at its CESK
--   machine state and figure out a single next step.
stepCESK :: (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m, Has Effect.Time sig m) => CESK -> m CESK
stepCESK cesk = case cesk of
  ------------------------------------------------------------
  -- Evaluation

  -- We wake up robots whose wake-up time has been reached. If it hasn't yet
  -- then stepCESK is a no-op.
  Waiting wakeupTime cesk' -> do
    time <- use $ temporal . ticks
    if wakeupTime <= time
      then stepCESK cesk'
      else return cesk
  Out v s (FImmediate cmd wf rf : k) ->
    processImmediateFrame v (SKpair s k) $
      updateWorldAndRobots cmd wf rf
  -- Now some straightforward cases.  These all immediately turn
  -- into values.
  In TUnit _ s k -> return $ Out VUnit s k
  In (TDir d) _ s k -> return $ Out (VDir d) s k
  In (TInt n) _ s k -> return $ Out (VInt n) s k
  In (TText str) _ s k -> return $ Out (VText str) s k
  In (TBool b) _ s k -> return $ Out (VBool b) s k
  -- There should not be any antiquoted variables left at this point.
  In (TAntiText v) _ s k ->
    return $ Up (Fatal (T.append "Antiquoted variable found at runtime: $str:" v)) s k
  In (TAntiInt v) _ s k ->
    return $ Up (Fatal (T.append "Antiquoted variable found at runtime: $int:" v)) s k
  -- Require and requireDevice just turn into no-ops.
  In (TRequireDevice {}) e s k -> return $ In (TConst Noop) e s k
  In (TRequire {}) e s k -> return $ In (TConst Noop) e s k
  In (TRequirements x t) e s k -> return $ Out (VRequirements x t e) s k
  -- Type ascriptions are ignored
  In (TAnnotate v _) e s k -> return $ In v e s k
  -- Normally it's not possible to have a TRobot value in surface
  -- syntax, but the salvage command generates a program that needs to
  -- refer directly to the salvaging robot.
  In (TRobot rid) _ s k -> return $ Out (VRobot rid) s k
  -- Function constants of arity 0 are evaluated immediately
  -- (e.g. parent, self).  Any other constant is turned into a VCApp,
  -- which is waiting for arguments and/or an FExec frame.
  In (TConst c) _ s k
    | arity c == 0 && not (isCmd c) -> evalConst c [] s k
    | otherwise -> return $ Out (VCApp c []) s k
  -- To evaluate a variable, just look it up in the context.
  In (TVar x) e s k -> withExceptions s k $ do
    v <-
      lookup x e
        `isJustOr` Fatal (T.unwords ["Undefined variable", x, "encountered while running the interpreter."])
    return $ Out v s k

  -- To evaluate a pair, start evaluating the first component.
  In (TPair t1 t2) e s k -> return $ In t1 e s (FSnd t2 e : k)
  -- Once that's done, evaluate the second component.
  Out v1 s (FSnd t2 e : k) -> return $ In t2 e s (FFst v1 : k)
  -- Finally, put the results together into a pair value.
  Out v2 s (FFst v1 : k) -> return $ Out (VPair v1 v2) s k
  -- Lambdas immediately turn into closures.
  In (TLam x _ t) e s k -> return $ Out (VClo x t e) s k
  -- To evaluate an application, start by focusing on the left-hand
  -- side and saving the argument for later.
  In (TApp t1 t2) e s k -> return $ In t1 e s (FArg t2 e : k)
  -- Once that's done, switch to evaluating the argument.
  Out v1 s (FArg t2 e : k) -> return $ In t2 e s (FApp v1 : k)
  -- We can evaluate an application of a closure in the usual way.
  Out v2 s (FApp (VClo x t e) : k) -> return $ In t (addBinding x v2 e) s k
  -- We can also evaluate an application of a constant by collecting
  -- arguments, eventually dispatching to evalConst for function
  -- constants.
  Out v2 s (FApp (VCApp c args) : k)
    | not (isCmd c)
        && arity c == length args + 1 ->
        evalConst c (reverse (v2 : args)) s k
    | otherwise -> return $ Out (VCApp c (v2 : args)) s k
  Out _ s (FApp _ : _) -> badMachineState s "FApp of non-function"
  -- Start evaluating a record.  If it's empty, we're done.  Otherwise, focus
  -- on the first field and record the rest in a FRcd frame.
  In (TRcd m) e s k -> return $ case M.assocs m of
    [] -> Out (VRcd M.empty) s k
    ((x, t) : fs) -> In (fromMaybe (TVar x) t) e s (FRcd e [] x fs : k)
  -- When we finish evaluating the last field, return a record value.
  Out v s (FRcd _ done x [] : k) -> return $ Out (VRcd (M.fromList ((x, v) : done))) s k
  -- Otherwise, save the value of the field just evaluated and move on
  -- to focus on evaluating the next one.
  Out v s (FRcd e done x ((y, t) : rest) : k) ->
    return $ In (fromMaybe (TVar y) t) e s (FRcd e ((x, v) : done) y rest : k)
  -- Evaluate a record projection: evaluate the record and remember we
  -- need to do the projection later.
  In (TProj t x) e s k -> return $ In t e s (FProj x : k)
  -- Do a record projection
  Out v s (FProj x : k) -> case v of
    VRcd m -> case M.lookup x m of
      Nothing -> badMachineState s $ T.unwords ["Record projection for variable", x, "that does not exist"]
      Just xv -> return $ Out xv s k
    _ -> badMachineState s "FProj frame with non-record value"
  -- To evaluate non-recursive let expressions, we start by focusing on the
  -- let-bound expression.
  In (TLet False x _ t1 t2) e s k -> return $ In t1 e s (FLet x t2 e : k)
  -- To evaluate recursive let expressions, we evaluate the memoized
  -- delay of the let-bound expression.  Every free occurrence of x
  -- in the let-bound expression and the body has already been
  -- rewritten by elaboration to 'force x'.
  In (TLet True x _ t1 t2) e s k ->
    return $ In (TDelay (MemoizedDelay $ Just x) t1) e s (FLet x t2 e : k)
  -- Once we've finished with the let-binding, we switch to evaluating
  -- the body in a suitably extended environment.
  Out v1 s (FLet x t2 e : k) -> return $ In t2 (addBinding x v1 e) s k
  -- Definitions immediately turn into VDef values, awaiting execution.
  In tm@(TDef r x _ t) e s k -> withExceptions s k $ do
    hasCapabilityFor CEnv tm
    return $ Out (VDef r x t e) s k

  -- Bind expressions don't evaluate: just package it up as a value
  -- until such time as it is to be executed.
  In (TBind mx t1 t2) e s k -> return $ Out (VBind mx t1 t2 e) s k
  -- Simple (non-memoized) delay expressions immediately turn into
  -- VDelay values, awaiting application of 'Force'.
  In (TDelay SimpleDelay t) e s k -> return $ Out (VDelay t e) s k
  -- For memoized delay expressions, we allocate a new cell in the store and
  -- return a reference to it.
  In (TDelay (MemoizedDelay x) t) e s k -> do
    -- Note that if the delay expression is recursive, we add a
    -- binding to the environment that wil be used to evaluate the
    -- body, binding the variable to a reference to the memory cell we
    -- just allocated for the body expression itself.  As a fun aside,
    -- notice how Haskell's recursion and laziness play a starring
    -- role: @loc@ is both an output from @allocate@ and used as part
    -- of an input! =D
    let (loc, s') = allocate (maybe id (`addBinding` VRef loc) x e) t s
    return $ Out (VRef loc) s' k
  -- If we see an update frame, it means we're supposed to set the value
  -- of a particular cell to the value we just finished computing.
  Out v s (FUpdate loc : k) -> return $ Out v (setStore loc (V v) s) k
  ------------------------------------------------------------
  -- Execution

  -- Executing a 'requirements' command generates an appropriate log message
  -- listing the requirements of the given expression.
  Out (VRequirements src t _) s (FExec : k) -> do
    currentContext <- use $ robotContext . defReqs
    em <- use $ landscape . entityMap
    let (R.Requirements caps devs inv, _) = R.requirements currentContext t

        devicesForCaps, requiredDevices :: Set (Set Text)
        -- possible devices to provide each required capability
        devicesForCaps = S.map (S.fromList . map (^. entityName) . (`deviceForCap` em)) caps
        -- outright required devices
        requiredDevices = S.map S.singleton devs

        deviceSets :: Set (Set Text)
        deviceSets =
          -- Union together all required device sets, and remove any
          -- device sets which are a superset of another set.  For
          -- example, if (grabber OR fast grabber OR harvester) is
          -- required but (grabber OR fast grabber) is also required
          -- then we might as well remove the first set, since
          -- satisfying the second device set will automatically
          -- satisfy the first.
          removeSupersets $ devicesForCaps `S.union` requiredDevices

        reqLog =
          prettyText $
            BulletList
              (pretty $ T.unwords ["Requirements for", bquote src <> ":"])
              ( filter
                  (not . null . bulletListItems)
                  [ BulletList
                      "Equipment:"
                      (T.intercalate " OR " . S.toList <$> S.toList deviceSets)
                  , BulletList
                      "Inventory:"
                      ((\(e, n) -> e <> " " <> parens (showT n)) <$> M.assocs inv)
                  ]
              )

    _ <- traceLog Logged Info reqLog
    return $ Out VUnit s k

  -- To execute a definition, we immediately turn the body into a
  -- delayed value, so it will not even be evaluated until it is
  -- called.  We memoize both recursive and non-recursive definitions,
  -- since the point of a definition is that it may be used many times.
  Out (VDef r x t e) s (FExec : k) ->
    return $ In (TDelay (MemoizedDelay $ bool Nothing (Just x) r) t) e s (FDef x : k)
  -- Once we have finished evaluating the (memoized, delayed) body of
  -- a definition, we return a special VResult value, which packages
  -- up the return value from the @def@ command itself (@unit@)
  -- together with the resulting environment (the variable bound to
  -- the delayed value).
  Out v s (FDef x : k) ->
    return $ Out (VResult VUnit (singleton x v)) s k
  -- To execute a constant application, delegate to the 'evalConst'
  -- function.  Set tickStepBudget to 0 if the command is supposed to take
  -- a tick, so the robot won't take any more steps this tick.
  Out (VCApp c args) s (FExec : k) -> do
    when (isTangible c) $ activityCounts . tickStepBudget .= 0
    evalConst c (reverse args) s k

  -- Reset the runningAtomic flag when we encounter an FFinishAtomic frame.
  Out v s (FFinishAtomic : k) -> do
    runningAtomic .= False
    return $ Out v s k

  -- Machinery for implementing the 'Swarm.Language.Syntax.MeetAll' command.
  -- First case: done meeting everyone.
  Out b s (FMeetAll _ [] : k) -> return $ Out b s k
  -- More still to meet: apply the function to the current value b and
  -- then the next robot id.  This will result in a command which we
  -- execute, discard any generated environment, and then pass the
  -- result to continue meeting the rest of the robots.
  Out b s (FMeetAll f (rid : rids) : k) ->
    return $ Out b s (FApp f : FArg (TRobot rid) empty : FExec : FDiscardEnv : FMeetAll f rids : k)
  -- To execute a bind expression, evaluate and execute the first
  -- command, and remember the second for execution later.
  Out (VBind mx c1 c2 e) s (FExec : k) -> return $ In c1 e s (FExec : FBind mx c2 e : k)
  -- If first command completes with a value along with an environment
  -- resulting from definition commands and/or binds, switch to
  -- evaluating the second command of the bind.  Extend the
  -- environment with both the environment resulting from the first
  -- command, as well as a binding for the result (if the bind was of
  -- the form @x <- c1; c2@).  Remember that we must execute the
  -- second command once it has been evaluated, then union any
  -- resulting definition environment with the definition environment
  -- from the first command.
  Out (VResult v ve) s (FBind mx t2 e : k) -> do
    let ve' = maybe id (`addBinding` v) mx ve
    return $ In t2 (e `union` ve') s (FExec : fUnionEnv ve' k)
  -- If the first command completes with a simple value and there is no binder,
  -- then we just continue without worrying about the environment.
  Out _ s (FBind Nothing t2 e : k) -> return $ In t2 e s (FExec : k)
  -- If the first command completes with a simple value and there is a binder,
  -- we promote it to the returned environment as well.
  Out v s (FBind (Just x) t2 e : k) -> do
    return $ In t2 (addBinding x v e) s (FExec : fUnionEnv (singleton x v) k)
  -- If a command completes with a value and definition environment,
  -- and the next continuation frame contains a previous environment
  -- to union with, then pass the unioned environments along in
  -- another VResult.

  Out (VResult v e2) s (FUnionEnv e1 : k) -> return $ Out (VResult v (e1 `union` e2)) s k
  -- Or, if a command completes with no environment, but there is a
  -- previous environment to union with, just use that environment.
  Out v s (FUnionEnv e : k) -> return $ Out (VResult v e) s k
  -- If there's an explicit DiscardEnv frame, throw away any returned environment.
  Out (VResult v _) s (FDiscardEnv : k) -> return $ Out v s k
  Out v s (FDiscardEnv : k) -> return $ Out v s k
  -- If the top of the continuation stack contains a 'FLoadEnv' frame,
  -- it means we are supposed to load up the resulting definition
  -- environment, store, and type and capability contexts into the robot's
  -- top-level environment and contexts, so they will be available to
  -- future programs.
  Out (VResult v e) s (FLoadEnv ctx rctx : k) -> do
    robotContext . defVals %= (`union` e)
    robotContext . defTypes %= (`union` ctx)
    robotContext . defReqs %= (`union` rctx)
    return $ Out v s k
  Out v s (FLoadEnv {} : k) -> return $ Out v s k
  -- Any other type of value wiwth an FExec frame is an error (should
  -- never happen).
  Out _ s (FExec : _) -> badMachineState s "FExec frame with non-executable value"
  -- If we see a VResult in any other context, simply discard it.  For
  -- example, this is what happens when there are binders (i.e. a "do
  -- block") nested inside another block instead of at the top level.
  -- It used to be that (1) only 'def' could generate a VResult, and
  -- (2) 'def' was guaranteed to only occur at the top level, hence
  -- any VResult would be caught by a FLoadEnv frame, and seeing a
  -- VResult anywhere else was an error.  But
  -- https://github.com/swarm-game/swarm/commit/b62d27e566565aa9a3ff351d91b23d2589b068dc
  -- made top-level binders export a variable binding, also via the
  -- VResult mechanism, and unlike 'def', binders do not have to occur
  -- at the top level only.  This led to
  -- https://github.com/swarm-game/swarm/issues/327 , which was fixed
  -- by changing this case from an error to simply ignoring the
  -- VResult wrapper.
  Out (VResult v _) s k -> return $ Out v s k
  ------------------------------------------------------------
  -- Exception handling
  ------------------------------------------------------------

  -- First, if we were running a try block but evaluation completed normally,
  -- just ignore the try block and continue.
  Out v s (FTry {} : k) -> return $ Out v s k
  Up exn s [] -> do
    -- Here, an exception has risen all the way to the top level without being
    -- handled.
    case exn of
      CmdFailed _ _ (Just a) -> do
        grantAchievement a
      _ -> return ()

    -- If an exception rises all the way to the top level without being
    -- handled, turn it into an error message.

    -- HOWEVER, we have to make sure to check that the robot has the
    -- 'log' capability which is required to collect and view logs.
    --
    -- Notice how we call resetBlackholes on the store, so that any
    -- cells which were in the middle of being evaluated will be reset.
    let s' = resetBlackholes s
    h <- hasCapability CLog
    em <- use $ landscape . entityMap
    if h
      then do
        void $ traceLog RobotError Error (formatExn em exn)
        return $ Out VUnit s []
      else return $ Out VUnit s' []
  -- Fatal errors, capability errors, and infinite loop errors can't
  -- be caught; just throw away the continuation stack.
  Up exn@Fatal {} s _ -> return $ Up exn s []
  Up exn@Incapable {} s _ -> return $ Up exn s []
  Up exn@InfiniteLoop {} s _ -> return $ Up exn s []
  -- Otherwise, if we are raising an exception up the continuation
  -- stack and come to a Try frame, force and then execute the associated catch
  -- block.
  Up _ s (FTry c : k) -> return $ Out c s (FApp (VCApp Force []) : FExec : k)
  -- Otherwise, keep popping from the continuation stack.
  Up exn s (_ : k) -> return $ Up exn s k
  -- Finally, if we're done evaluating and the continuation stack is
  -- empty, return the machine unchanged.
  done@(Out _ _ []) -> return done
 where
  badMachineState s msg =
    let msg' =
          T.unlines
            [ T.append "Bad machine state in stepRobot: " msg
            , prettyText cesk
            ]
     in return $ Up (Fatal msg') s []

  -- Note, the order of arguments to `union` is important in the below
  -- definition of fUnionEnv.  I wish I knew how to add an automated
  -- test for this.  But you can tell the difference in the following
  -- REPL session:
  --
  -- > x <- return 1; x <- return 2
  -- 2 : int
  -- > x
  -- 2 : int
  --
  -- If we switch the code to read 'e1 `union` e2' instead, then
  -- the first expression above still correctly evaluates to 2, but
  -- x ends up incorrectly bound to 1.

  fUnionEnv e1 = \case
    FUnionEnv e2 : k -> FUnionEnv (e2 `union` e1) : k
    k -> FUnionEnv e1 : k

-- | Execute the given program *hypothetically*: i.e. in a fresh
-- CESK machine, using *copies* of the current store, robot
-- and game state.  We discard the state afterwards so any
-- modifications made by prog do not persist.  Note we also
-- set the copied robot to be a "system" robot so it is
-- capable of executing any commands; the As command
-- already requires "God" capability.
runChildProg ::
  (HasRobotStepState sig m, Has Effect.Time sig m, Has (Lift IO) sig m) =>
  Store ->
  Robot ->
  Value ->
  m Value
runChildProg s r prog = do
  g <- get @GameState
  evalState @Robot (r & systemRobot .~ True) . evalState @GameState g $
    runCESK (Out prog s [FApp (VCApp Force []), FExec])

-- | Execute a constant, catching any exception thrown and returning
--   it via a CESK machine state.
evalConst ::
  (Has (State GameState) sig m, Has (State Robot) sig m, Has Effect.Time sig m, Has (Lift IO) sig m) => Const -> [Value] -> Store -> Cont -> m CESK
evalConst c vs s k = do
  res <- runError $ execConst runChildProg c vs s k
  case res of
    Left exn -> return $ Up exn s k
    Right cek' -> return cek'
