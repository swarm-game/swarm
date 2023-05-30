{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Facilities for stepping the robot CESK machines, /i.e./ the actual
-- interpreter for the Swarm language.
--
-- ** Note on the IO:
--
-- The only reason we need @IO@ is so that robots can run programs
-- loaded from files, via the 'Run' command.
-- This could be avoided by using 'Import' command instead and parsing
-- the required files at the time of declaration.
-- See <https://github.com/swarm-game/swarm/issues/495>.
module Swarm.Game.Step where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Carrier.Error.Either (ErrorC, runError)
import Control.Carrier.State.Lazy
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Lens as Lens hiding (Const, distrib, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Control.Monad (foldM, forM, forM_, guard, msum, unless, when, zipWithM)
import Control.Monad.Except (runExceptT)
import Data.Array (bounds, (!))
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Char (chr, ord)
import Data.Either (partitionEithers, rights)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (asum, for_, traverse_)
import Data.Foldable.Extra (findM, firstJustM)
import Data.Function (on)
import Data.Functor (void)
import Data.Int (Int32)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List (find, sortOn)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Sequence ((><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Data.Tuple (swap)
import Linear (V2 (..), perp, zero)
import Prettyprinter (pretty)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Exception
import Swarm.Game.Failure
import Swarm.Game.Location
import Swarm.Game.Recipe
import Swarm.Game.ResourceLoading (getDataFileNameSafe)
import Swarm.Game.Robot
import Swarm.Game.Scenario.Objective qualified as OB
import Swarm.Game.Scenario.Objective.WinCheck qualified as WC
import Swarm.Game.State
import Swarm.Game.Value
import Swarm.Game.World qualified as W
import Swarm.Language.Capability
import Swarm.Language.Context hiding (delete)
import Swarm.Language.Key (parseKeyComboFull)
import Swarm.Language.Parse (runParser)
import Swarm.Language.Pipeline
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Pretty (BulletList (BulletList, bulletListItems), prettyText)
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Value
import Swarm.Util hiding (both)
import System.Clock (TimeSpec)
import System.Clock qualified
import System.Random (UniformRange, uniformR)
import Witch (From (from), into)
import Prelude hiding (lookup)

-- | The main function to do one game tick.
--
--   Note that the game may be in 'RobotStep' mode and not finish
--   the tick. Use the return value to check whether a full tick happened.
gameTick :: (Has (State GameState) sig m, Has (Lift IO) sig m) => m Bool
gameTick = do
  wakeUpRobotsDoneSleeping
  active <- use activeRobots
  focusedRob <- use focusedRobotID

  ticked <-
    use gameStep >>= \case
      WorldTick -> do
        runRobotIDs active
        ticks += 1
        pure True
      RobotStep ss -> singleStep ss focusedRob active

  -- See if the base is finished with a computation, and if so, record
  -- the result in the game state so it can be displayed by the REPL;
  -- also save the current store into the robotContext so we can
  -- restore it the next time we start a computation.
  mr <- use (robotMap . at 0)
  case mr of
    Just r -> do
      res <- use replStatus
      case res of
        REPLWorking (Typed Nothing ty req) -> case getResult r of
          Just (v, s) -> do
            replStatus .= REPLWorking (Typed (Just v) ty req)
            baseRobot . robotContext . defStore .= s
          Nothing -> pure ()
        _otherREPLStatus -> pure ()
    Nothing -> pure ()

  -- Possibly update the view center.
  modify recalcViewCenter

  when ticked $ do
    -- On new tick see if the winning condition for the current objective is met.
    wc <- use winCondition
    case wc of
      WinConditions winState oc -> do
        g <- get @GameState
        em <- use entityMap
        hypotheticalWinCheck em g winState oc
      _ -> pure ()
  return ticked

-- | Finish a game tick in progress and set the game to 'WorldTick' mode afterwards.
--
-- Use this function if you need to unpause the game.
finishGameTick :: (Has (State GameState) sig m, Has (Lift IO) sig m) => m ()
finishGameTick =
  use gameStep >>= \case
    WorldTick -> pure ()
    RobotStep SBefore -> gameStep .= WorldTick
    RobotStep _ -> void gameTick >> finishGameTick

-- Insert the robot back to robot map.
-- Will selfdestruct or put the robot to sleep if it has that set.
insertBackRobot :: Has (State GameState) sig m => RID -> Robot -> m ()
insertBackRobot rn rob = do
  time <- use ticks
  if rob ^. selfDestruct
    then deleteRobot rn
    else do
      robotMap %= IM.insert rn rob
      case waitingUntil rob of
        Just wakeUpTime
          -- if w=2 t=1 then we do not needlessly put robot to waiting queue
          | wakeUpTime - 2 <= time -> return ()
          | otherwise -> sleepUntil rn wakeUpTime
        Nothing ->
          unless (isActive rob) (sleepForever rn)

-- Run a set of robots - this is used to run robots before/after the focused one.
runRobotIDs :: (Has (State GameState) sig m, Has (Lift IO) sig m) => IS.IntSet -> m ()
runRobotIDs robotNames = forM_ (IS.toList robotNames) $ \rn -> do
  mr <- uses robotMap (IM.lookup rn)
  forM_ mr (stepOneRobot rn)
 where
  stepOneRobot rn rob = tickRobot rob >>= insertBackRobot rn

-- This is a helper function to do one robot step or run robots before/after.
singleStep :: (Has (State GameState) sig m, Has (Lift IO) sig m) => SingleStep -> RID -> IS.IntSet -> m Bool
singleStep ss focRID robotSet = do
  let (preFoc, focusedActive, postFoc) = IS.splitMember focRID robotSet
  case ss of
    ----------------------------------------------------------------------------
    -- run robots from the beginning until focused robot
    SBefore -> do
      runRobotIDs preFoc
      gameStep .= RobotStep (SSingle focRID)
      -- also set ticks of focused robot
      steps <- use robotStepsPerTick
      robotMap . ix focRID . tickSteps .= steps
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
      mOldR <- uses robotMap (IM.lookup focRID)
      case mOldR of
        Nothing | rid == focRID -> do
          debugLog "The debugged robot does not exist! Exiting single step mode."
          runRobotIDs postFoc
          gameStep .= WorldTick
          ticks += 1
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
              when (newR ^. tickSteps == 0) $ gameStep .= RobotStep (SAfter focRID)
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
      gameStep .= RobotStep SBefore
      ticks += 1
      return True
    SAfter rid | otherwise -> do
      -- go to single step if new robot is focused
      let (_pre, postRID) = IS.split rid robotSet
      singleStep SBefore focRID postRID
 where
  h = hypotheticalRobot (Out VUnit emptyStore []) 0
  debugLog txt = do
    m <- evalState @Robot h $ createLogEntry (ErrorTrace Debug) txt
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
  (Has (State GameState) sig m, Has (Lift IO) sig m) =>
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

  announcementQueue %= (>< Seq.fromList (map ObjectiveCompleted $ completionAnnouncementQueue finalAccumulator))

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
    m <- evalState @Robot h $ createLogEntry (ErrorTrace Critical) exnText
    emitMessage m
   where
    h = hypotheticalRobot (Out VUnit emptyStore []) 0

evalPT ::
  (Has (Lift IO) sig m, Has (Throw Exn) sig m, Has (State GameState) sig m) =>
  ProcessedTerm ->
  m Value
evalPT t = evaluateCESK (initMachine t empty emptyStore)

getNow :: Has (Lift IO) sig m => m TimeSpec
getNow = sendIO $ System.Clock.getTime System.Clock.Monotonic

-- | Create a special robot to check some hypothetical, for example the win condition.
--
-- Use ID (-1) so it won't conflict with any robots currently in the robot map.
hypotheticalRobot :: CESK -> TimeSpec -> Robot
hypotheticalRobot c = mkRobot (-1) Nothing "hypothesis" [] zero zero defaultRobotDisplay c [] [] True False

evaluateCESK ::
  (Has (Lift IO) sig m, Has (Throw Exn) sig m, Has (State GameState) sig m) =>
  CESK ->
  m Value
evaluateCESK cesk = do
  createdAt <- getNow
  let r = hypotheticalRobot cesk createdAt
  addRobot r -- Add the special robot to the robot map, so it can look itself up if needed
  evalState r . runCESK $ cesk

runCESK ::
  ( Has (Lift IO) sig m
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
-- Some utility functions
------------------------------------------------------------

-- | Set a flag telling the UI that the world needs to be redrawn.
flagRedraw :: (Has (State GameState) sig m) => m ()
flagRedraw = needsRedraw .= True

-- | Perform an action requiring a 'W.World' state component in a
--   larger context with a 'GameState'.
zoomWorld :: (Has (State GameState) sig m) => StateC (W.World Int Entity) Identity b -> m b
zoomWorld n = do
  w <- use world
  let (w', a) = run (runState w n)
  world .= w'
  return a

-- | Get the entity (if any) at a given location.
entityAt :: (Has (State GameState) sig m) => Location -> m (Maybe Entity)
entityAt loc = zoomWorld (W.lookupEntityM @Int (W.locToCoords loc))

-- | Modify the entity (if any) at a given location.
updateEntityAt ::
  (Has (State GameState) sig m) => Location -> (Maybe Entity -> Maybe Entity) -> m ()
updateEntityAt loc upd = do
  didChange <- zoomWorld $ W.updateM @Int (W.locToCoords loc) upd
  when didChange $
    wakeWatchingRobots loc

-- | Get the robot with a given ID.
robotWithID :: (Has (State GameState) sig m) => RID -> m (Maybe Robot)
robotWithID rid = use (robotMap . at rid)

-- | Get the robot with a given name.
robotWithName :: (Has (State GameState) sig m) => Text -> m (Maybe Robot)
robotWithName rname = use (robotMap . to IM.elems . to (find $ \r -> r ^. robotName == rname))

-- | Generate a uniformly random number using the random generator in
--   the game state.
uniform :: (Has (State GameState) sig m, UniformRange a) => (a, a) -> m a
uniform bnds = do
  rand <- use randGen
  let (n, g) = uniformR bnds rand
  randGen .= g
  return n

-- | Given a weighting function and a list of values, choose one of
--   the values randomly (using the random generator in the game
--   state), with the probability of each being proportional to its
--   weight.  Return @Nothing@ if the list is empty.
weightedChoice :: Has (State GameState) sig m => (a -> Integer) -> [a] -> m (Maybe a)
weightedChoice weight as = do
  r <- uniform (0, total - 1)
  return $ go r as
 where
  total = sum (map weight as)

  go _ [] = Nothing
  go !k (x : xs)
    | k < w = Just x
    | otherwise = go (k - w) xs
   where
    w = weight x

-- | Generate a random robot name in the form adjective_name.
randomName :: Has (State GameState) sig m => m Text
randomName = do
  adjs <- use @GameState adjList
  names <- use @GameState nameList
  i <- uniform (bounds adjs)
  j <- uniform (bounds names)
  return $ T.concat [adjs ! i, "_", names ! j]

------------------------------------------------------------
-- Debugging
------------------------------------------------------------

-- | Create a log entry given current robot and game time in ticks noting whether it has been said.
--
--   This is the more generic version used both for (recorded) said messages and normal logs.
createLogEntry :: (Has (State GameState) sig m, Has (State Robot) sig m) => LogSource -> Text -> m LogEntry
createLogEntry source msg = do
  rid <- use robotID
  rn <- use robotName
  time <- use ticks
  loc <- use robotLocation
  pure $ LogEntry time source rn rid loc msg

-- | Print some text via the robot's log.
traceLog :: (Has (State GameState) sig m, Has (State Robot) sig m) => LogSource -> Text -> m LogEntry
traceLog source msg = do
  m <- createLogEntry source msg
  robotLog %= (Seq.|> m)
  return m

-- | Print a showable value via the robot's log.
--
-- Useful for debugging.
traceLogShow :: (Has (State GameState) sig m, Has (State Robot) sig m, Show a) => a -> m ()
traceLogShow = void . traceLog Logged . from . show

------------------------------------------------------------
-- Exceptions and validation
------------------------------------------------------------

-- | Capabilities needed for a specific robot to evaluate or execute a
--   constant.  Right now, the only difference is whether the robot is
--   heavy or not when executing the 'Move' command, but there might
--   be other exceptions added in the future.
constCapsFor :: Const -> Robot -> Maybe Capability
constCapsFor Move r
  | r ^. robotHeavy = Just CMoveheavy
constCapsFor c _ = constCaps c

-- | Ensure that a robot is capable of executing a certain constant
--   (either because it has a device which gives it that capability,
--   or it is a system robot, or we are in creative mode).
ensureCanExecute :: (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Const -> m ()
ensureCanExecute c =
  gets @Robot (constCapsFor c) >>= \case
    Nothing -> pure ()
    Just cap -> do
      isPrivileged <- isPrivilegedBot
      robotCaps <- use robotCapabilities
      let hasCaps = cap `S.member` robotCaps
      (isPrivileged || hasCaps)
        `holdsOr` Incapable FixByEquip (R.singletonCap cap) (TConst c)

-- | Test whether the current robot has a given capability (either
--   because it has a device which gives it that capability, or it is a
--   system robot, or we are in creative mode).
hasCapability :: (Has (State Robot) sig m, Has (State GameState) sig m) => Capability -> m Bool
hasCapability cap = do
  isPrivileged <- isPrivilegedBot
  caps <- use robotCapabilities
  return (isPrivileged || cap `S.member` caps)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityFor ::
  (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Capability -> Term -> m ()
hasCapabilityFor cap term = do
  h <- hasCapability cap
  h `holdsOr` Incapable FixByEquip (R.singletonCap cap) term

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts) Nothing

-- | Create an exception about a command failing, with an achievement
cmdExnWithAchievement :: Const -> [Text] -> GameplayAchievement -> Exn
cmdExnWithAchievement c parts a = CmdFailed c (T.unwords parts) $ Just a

-- | Raise an exception about a command failing with a formatted error message.
raise :: (Has (Throw Exn) sig m) => Const -> [Text] -> m a
raise c parts = throwError (cmdExn c parts)

-- | Run a subcomputation that might throw an exception in a context
--   where we are returning a CESK machine; any exception will be
--   turned into an 'Up' state.
withExceptions :: Monad m => Store -> Cont -> ThrowC Exn m CESK -> m CESK
withExceptions s k m = do
  res <- runThrow m
  case res of
    Left exn -> return $ Up exn s k
    Right a -> return a

------------------------------------------------------------
-- Stepping robots
------------------------------------------------------------

-- | Run a robot for one tick, which may consist of up to
--   'robotStepsPerTick' CESK machine steps and at most one tangible
--   command execution, whichever comes first.
tickRobot :: (Has (State GameState) sig m, Has (Lift IO) sig m) => Robot -> m Robot
tickRobot r = do
  steps <- use robotStepsPerTick
  tickRobotRec (r & tickSteps .~ steps)

-- | Recursive helper function for 'tickRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
tickRobotRec :: (Has (State GameState) sig m, Has (Lift IO) sig m) => Robot -> m Robot
tickRobotRec r
  | isActive r && (r ^. runningAtomic || r ^. tickSteps > 0) =
      stepRobot r >>= tickRobotRec
  | otherwise = return r

-- | Single-step a robot by decrementing its 'tickSteps' counter and
--   running its CESK machine for one step.
stepRobot :: (Has (State GameState) sig m, Has (Lift IO) sig m) => Robot -> m Robot
stepRobot r = do
  (r', cesk') <- runState (r & tickSteps -~ 1) (stepCESK (r ^. machine))
  -- sendIO $ appendFile "out.txt" (prettyString cesk' ++ "\n")
  return $ r' & machine .~ cesk'

-- | replace some entity in the world with another entity
updateWorld ::
  (Has (State GameState) sig m, Has (Throw Exn) sig m) =>
  Const ->
  WorldUpdate Entity ->
  m ()
updateWorld c (ReplaceEntity loc eThen down) = do
  w <- use world
  let eNow = W.lookupEntity (W.locToCoords loc) w
  -- Can fail if a robot started a multi-tick "drill" operation on some entity
  -- and meanwhile another entity swaps it out from under them.
  if Just eThen /= eNow
    then throwError $ cmdExn c ["The", eThen ^. entityName, "is not there."]
    else updateEntityAt loc $ const down

applyRobotUpdates ::
  (Has (State GameState) sig m, Has (State Robot) sig m) =>
  [RobotUpdate] ->
  m ()
applyRobotUpdates =
  mapM_ \case
    AddEntity c e -> robotInventory %= E.insertCount c e
    LearnEntity e -> robotInventory %= E.insertCount 0 e

data SKpair = SKpair Store Cont

-- | Performs some side-effectful computation
-- for an "FImmediate" Frame.
-- Aborts processing the continuation stack
-- if an error is encountered.
--
-- Compare to "withExceptions".
processImmediateFrame ::
  (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m) =>
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

updateWorldAndRobots ::
  (HasRobotStepState sig m) =>
  Const ->
  [WorldUpdate Entity] ->
  [RobotUpdate] ->
  m ()
updateWorldAndRobots cmd wf rf = do
  mapM_ (updateWorld cmd) wf
  applyRobotUpdates rf
  flagRedraw

-- | The main CESK machine workhorse.  Given a robot, look at its CESK
--   machine state and figure out a single next step.
stepCESK :: (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m) => CESK -> m CESK
stepCESK cesk = case cesk of
  ------------------------------------------------------------
  -- Evaluation

  -- We wake up robots whose wake-up time has been reached. If it hasn't yet
  -- then stepCESK is a no-op.
  Waiting wakeupTime cesk' -> do
    time <- use ticks
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
  Out v s (FUpdate loc : k) -> return $ Out v (setCell loc (V v) s) k
  ------------------------------------------------------------
  -- Execution

  -- Executing a 'requirements' command generates an appropriate log message
  -- listing the requirements of the given expression.
  Out (VRequirements src t _) s (FExec : k) -> do
    currentContext <- use $ robotContext . defReqs
    em <- use entityMap
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

    _ <- traceLog Logged reqLog
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
  -- function.  Set tickSteps to 0 if the command is supposed to take
  -- a tick, so the robot won't take any more steps this tick.
  Out (VCApp c args) s (FExec : k) -> do
    when (isTangible c) $ tickSteps .= 0
    evalConst c (reverse args) s k

  -- Reset the runningAtomic flag when we encounter an FFinishAtomic frame.
  Out v s (FFinishAtomic : k) -> do
    runningAtomic .= False
    return $ Out v s k

  -- Machinery for implementing the 'meetAll' command.
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
    em <- use entityMap
    if h
      then do
        void $ traceLog (ErrorTrace Error) (formatExn em exn)
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

-- | Eexecute a constant, catching any exception thrown and returning
--   it via a CESK machine state.
evalConst ::
  (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m) => Const -> [Value] -> Store -> Cont -> m CESK
evalConst c vs s k = do
  res <- runError $ execConst c vs s k
  case res of
    Left exn -> return $ Up exn s k
    Right cek' -> return cek'

-- | A system program for a "seed robot", to regrow a growable entity
--   after it is harvested.
seedProgram :: Integer -> Integer -> Text -> ProcessedTerm
seedProgram minTime randTime thing =
  [tmQ|
    try {
      r <- random (1 + $int:randTime);
      wait (r + $int:minTime);
      appear "|";
      r <- random (1 + $int:randTime);
      wait (r + $int:minTime);
      place $str:thing;
    } {};
    selfdestruct
  |]

-- | Construct a "seed robot" from entity, time range and position,
--   and add it to the world.  It has low priority and will be covered
--   by placed entities.
addSeedBot :: Has (State GameState) sig m => Entity -> (Integer, Integer) -> Location -> TimeSpec -> m ()
addSeedBot e (minT, maxT) loc ts =
  void $
    addTRobot $
      mkRobot
        ()
        Nothing
        "seed"
        ["A growing seed."]
        (Just loc)
        zero
        ( defaultEntityDisplay '.'
            & displayAttr .~ (e ^. entityDisplay . displayAttr)
            & displayPriority .~ 0
        )
        (initMachine (seedProgram minT (maxT - minT) (e ^. entityName)) empty emptyStore)
        []
        [(1, e)]
        True
        False
        ts

-- | All functions that are used for robot step can access 'GameState' and the current 'Robot'.
--
-- They can also throw exception of our custom type, which is handled elsewhere.
-- Because of that the constraint is only 'Throw', but not 'Catch'/'Error'.
type HasRobotStepState sig m = (Has (State GameState) sig m, Has (State Robot) sig m, Has (Throw Exn) sig m)

-- | Interpret the execution (or evaluation) of a constant application
--   to some values.
execConst ::
  (HasRobotStepState sig m, Has (Lift IO) sig m) =>
  Const ->
  [Value] ->
  Store ->
  Cont ->
  m CESK
execConst c vs s k = do
  -- First, ensure the robot is capable of executing/evaluating this constant.
  ensureCanExecute c

  -- Now proceed to actually carry out the operation.
  case c of
    Noop -> return $ Out VUnit s k
    Return -> case vs of
      [v] -> return $ Out v s k
      _ -> badConst
    Wait -> case vs of
      [VInt d] -> do
        time <- use ticks
        purgeFarAwayWatches
        return $ Waiting (time + d) (Out VUnit s k)
      _ -> badConst
    Selfdestruct -> do
      destroyIfNotBase $ Just AttemptSelfDestructBase
      flagRedraw
      return $ Out VUnit s k
    Move -> do
      -- Figure out where we're going
      loc <- use robotLocation
      orient <- use robotOrientation
      let nextLoc = loc .+^ (orient ? zero)
      checkMoveAhead nextLoc $
        MoveFailure
          { failIfBlocked = ThrowExn
          , failIfDrown = Destroy
          }
      updateRobotLocation loc nextLoc
      return $ Out VUnit s k
    Push -> do
      -- Figure out where we're going
      loc <- use robotLocation
      orient <- use robotOrientation
      let heading = orient ? zero
          nextLoc = loc .+^ heading
          placementLoc = nextLoc .+^ heading

      -- If unobstructed, the robot will move even if
      -- there is nothing to push.
      maybeCurrentE <- entityAt nextLoc
      case maybeCurrentE of
        Just e -> do
          -- Make sure there's nothing already occupying the destination
          nothingHere <- isNothing <$> entityAt placementLoc
          nothingHere `holdsOrFail` ["Something is in the way!"]

          let verbed = verbedGrabbingCmd Push'
          -- Ensure it can be pushed.
          omni <- isPrivilegedBot
          (omni || e `hasProperty` Portable && not (e `hasProperty` Liquid))
            `holdsOrFail` ["The", e ^. entityName, "here can't be", verbed <> "."]

          -- Place the entity and remove it from previous loc
          updateEntityAt nextLoc (const Nothing)
          updateEntityAt placementLoc (const (Just e))
        Nothing -> return ()

      updateRobotLocation loc nextLoc
      return $ Out VUnit s k
    Stride -> case vs of
      [VInt d] -> do
        when (d > fromIntegral maxStrideRange) $
          throwError $
            CmdFailed
              Stride
              ( T.unwords
                  [ "Can only stride up to"
                  , T.pack $ show maxStrideRange
                  , "units."
                  ]
              )
              Nothing

        -- Figure out where we're going
        loc <- use robotLocation
        orient <- use robotOrientation
        let heading = orient ? zero

        -- Excludes the base location.
        let locsInDirection :: [Location]
            locsInDirection =
              take (min (fromIntegral d) maxStrideRange) $
                drop 1 $
                  iterate (.+^ heading) loc

        failureMaybes <- mapM checkMoveFailure locsInDirection
        let maybeFirstFailure = asum failureMaybes

        applyMoveFailureEffect maybeFirstFailure $
          MoveFailure
            { failIfBlocked = ThrowExn
            , failIfDrown = Destroy
            }

        let maybeLastLoc = do
              guard $ null maybeFirstFailure
              listToMaybe $ reverse locsInDirection

        forM_ maybeLastLoc $ updateRobotLocation loc

        return $ Out VUnit s k
      _ -> badConst
    Teleport -> case vs of
      [VRobot rid, VPair (VInt x) (VInt y)] -> do
        -- Make sure the other robot exists and is close
        target <- getRobotWithinTouch rid
        -- either change current robot or one in robot map
        let oldLoc = target ^. robotLocation
            nextLoc = Location (fromIntegral x) (fromIntegral y)

        onTarget rid $ do
          checkMoveAhead nextLoc $
            MoveFailure
              { failIfBlocked = Destroy
              , failIfDrown = Destroy
              }
          updateRobotLocation oldLoc nextLoc

        return $ Out VUnit s k
      _ -> badConst
    Grab -> doGrab Grab'
    Harvest -> doGrab Harvest'
    Swap -> case vs of
      [VText name] -> do
        loc <- use robotLocation
        -- Make sure the robot has the thing in its inventory
        e <- hasInInventoryOrFail name
        -- Grab
        r <- doGrab Swap'
        case r of
          Out {} -> do
            -- Place the entity and remove it from the inventory
            updateEntityAt loc (const (Just e))
            robotInventory %= delete e
          _ -> pure ()
        return r
      _ -> badConst
    Turn -> case vs of
      [VDir d] -> do
        when (isCardinal d) $ hasCapabilityFor COrient (TDir d)
        robotOrientation . _Just %= applyTurn d
        flagRedraw

        inst <- use equippedDevices
        when (d == DRelative DDown && countByName "compass" inst == 0) $ do
          grantAchievement GetDisoriented

        return $ Out VUnit s k
      _ -> badConst
    Place -> case vs of
      [VText name] -> do
        loc <- use robotLocation

        -- Make sure there's nothing already here
        nothingHere <- isNothing <$> entityAt loc
        nothingHere `holdsOrFail` ["There is already an entity here."]

        -- Make sure the robot has the thing in its inventory
        e <- hasInInventoryOrFail name

        -- Place the entity and remove it from the inventory
        updateEntityAt loc (const (Just e))
        robotInventory %= delete e

        flagRedraw
        return $ Out VUnit s k
      _ -> badConst
    Give -> case vs of
      [VRobot otherID, VText itemName] -> do
        -- Make sure the other robot exists and is close
        _other <- getRobotWithinTouch otherID

        item <- ensureItem itemName "give"

        -- Giving something to ourself should be a no-op.  We need
        -- this as a special case since it will not work to modify
        -- ourselves in the robotMap --- after performing a tick we
        -- return a modified Robot which gets put back in the
        -- robotMap, overwriting any changes to this robot made
        -- directly in the robotMap during the tick.
        myID <- use robotID
        focusedID <- use focusedRobotID
        when (otherID /= myID) $ do
          -- Make the exchange
          robotMap . at otherID . _Just . robotInventory %= insert item
          robotInventory %= delete item

          -- Flag the UI for a redraw if we are currently showing either robot's inventory
          when (focusedID == myID || focusedID == otherID) flagRedraw

        return $ Out VUnit s k
      _ -> badConst
    Equip -> case vs of
      [VText itemName] -> do
        item <- ensureItem itemName "equip"
        myID <- use robotID
        focusedID <- use focusedRobotID
        -- Don't do anything if the robot already has the device.
        already <- use (equippedDevices . to (`E.contains` item))
        unless already $ do
          equippedDevices %= insert item
          robotInventory %= delete item

          -- Flag the UI for a redraw if we are currently showing our inventory
          when (focusedID == myID) flagRedraw

        return $ Out VUnit s k
      _ -> badConst
    Unequip -> case vs of
      [VText itemName] -> do
        item <- ensureEquipped itemName
        myID <- use robotID
        focusedID <- use focusedRobotID
        equippedDevices %= delete item
        robotInventory %= insert item
        -- Flag the UI for a redraw if we are currently showing our inventory
        when (focusedID == myID) flagRedraw
        return $ Out VUnit s k
      _ -> badConst
    Make -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        ins <- use equippedDevices
        em <- use entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name <> "."]

        outRs <- use recipesOut

        creative <- use creativeMode
        let create l = l <> ["You can use 'create \"" <> name <> "\"' instead." | creative]

        -- Only consider recipes where the number of things we are trying to make
        -- is greater in the outputs than in the inputs.  This prevents us from doing
        -- silly things like making copper pipes when the user says "make furnace".
        let recipes = filter increase (recipesFor outRs e)
            increase r = countIn (r ^. recipeOutputs) > countIn (r ^. recipeInputs)
            countIn xs = maybe 0 fst (find ((== e) . snd) xs)
        not (null recipes)
          `holdsOrFail` create ["There is no known recipe for making", indefinite name <> "."]

        let displayMissingCount mc = \case
              MissingInput -> from (show mc)
              MissingCatalyst -> "not equipped"
            displayMissingIngredient (MissingIngredient mk mc me) =
              "  - " <> me ^. entityName <> " (" <> displayMissingCount mc mk <> ")"
            displayMissingIngredients xs = L.intercalate ["OR"] (map displayMissingIngredient <$> xs)

        -- Try recipes and make a weighted random choice among the
        -- ones we have ingredients for.
        let (badRecipes, goodRecipes) = partitionEithers . map (make (inv, ins)) $ recipes
        chosenRecipe <- weightedChoice (^. _3 . recipeWeight) goodRecipes
        (invTaken, changeInv, recipe) <-
          chosenRecipe
            `isJustOrFail` create
              [ "You don't have the ingredients to make"
              , indefinite name <> "."
              , "Missing:\n" <> T.unlines (displayMissingIngredients badRecipes)
              ]

        -- take recipe inputs from inventory and add outputs after recipeTime
        robotInventory .= invTaken
        traverse_ (updateDiscoveredEntities . snd) (recipe ^. recipeOutputs)
        finishCookingRecipe recipe VUnit [] (map (uncurry AddEntity) changeInv)
      _ -> badConst
    Has -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        return $ Out (VBool ((> 0) $ countByName name inv)) s k
      _ -> badConst
    Equipped -> case vs of
      [VText name] -> do
        inv <- use equippedDevices
        return $ Out (VBool ((> 0) $ countByName name inv)) s k
      _ -> badConst
    Count -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        return $ Out (VInt (fromIntegral $ countByName name inv)) s k
      _ -> badConst
    Scout -> case vs of
      [VDir d] -> do
        rMap <- use robotMap
        myLoc <- use robotLocation
        heading <- deriveHeading d
        botsByLocs <- use robotsByLocation
        selfRid <- use robotID

        -- Includes the base location, so we exclude the base robot later.
        let locsInDirection :: [Location]
            locsInDirection = take maxScoutRange $ iterate (.+^ heading) myLoc

        let hasOpaqueEntity =
              fmap (maybe False (`hasProperty` E.Opaque)) . entityAt

        let hasVisibleBot :: Location -> Bool
            hasVisibleBot = any botIsVisible . IS.toList . excludeSelf . botsHere
             where
              excludeSelf = (`IS.difference` IS.singleton selfRid)
              botsHere loc = M.findWithDefault mempty loc botsByLocs
              botIsVisible = maybe False canSee . (`IM.lookup` rMap)
              canSee = not . (^. robotDisplay . invisible)

        -- A robot on the same cell as an opaque entity is considered hidden.
        -- Returns (Just Bool) if the result is conclusively visible or opaque,
        -- or Nothing if we don't have a conclusive answer yet.
        let isConclusivelyVisible :: Bool -> Location -> Maybe Bool
            isConclusivelyVisible isOpaque loc
              | isOpaque = Just False
              | hasVisibleBot loc = Just True
              | otherwise = Nothing

        let isConclusivelyVisibleM loc = do
              opaque <- hasOpaqueEntity loc
              return $ isConclusivelyVisible opaque loc

        -- This ensures that we only evaluate locations until
        -- a conclusive result is obtained, so we don't always
        -- have to inspect the maximum range of the command.
        result <- firstJustM isConclusivelyVisibleM locsInDirection
        let foundBot = fromMaybe False result
        return $ Out (VBool foundBot) s k
      _ -> badConst
    Whereami -> do
      loc <- use robotLocation
      return $ Out (asValue loc) s k
    Detect -> case vs of
      [VText name, VRect x1 y1 x2 y2] -> do
        loc <- use robotLocation
        let locs = rectCells x1 y1 x2 y2
        -- sort offsets by (Manhattan) distance so that we return the closest occurrence
        let sortedLocs = sortOn (\(V2 x y) -> abs x + abs y) locs
        firstOne <- findM (fmap (maybe False $ isEntityNamed name) . entityAt . (loc .+^)) sortedLocs
        return $ Out (asValue firstOne) s k
      _ -> badConst
    Resonate -> case vs of
      [VText name, VRect x1 y1 x2 y2] -> do
        loc <- use robotLocation
        let locs = rectCells x1 y1 x2 y2
        hits <- mapM (fmap (fromEnum . maybe False (isEntityNamed name)) . entityAt . (loc .+^)) locs
        return $ Out (VInt $ fromIntegral $ sum hits) s k
      _ -> badConst
    Sniff -> case vs of
      [VText name] -> do
        firstFound <- findNearest name
        return $ Out (asValue $ maybe (-1) fst firstFound) s k
      _ -> badConst
    Watch -> case vs of
      [VDir d] -> do
        (loc, _me) <- lookInDirection d
        addWatchedLocation loc
        return $ Out VUnit s k
      _ -> badConst
    Surveil -> case vs of
      [VPair (VInt x) (VInt y)] -> do
        let loc = Location (fromIntegral x) (fromIntegral y)
        addWatchedLocation loc
        return $ Out VUnit s k
      _ -> badConst
    Chirp -> case vs of
      [VText name] -> do
        firstFound <- findNearest name
        mh <- use robotOrientation
        inst <- use equippedDevices
        let processDirection entityDir =
              if countByName "compass" inst >= 1
                then Just $ DAbsolute entityDir
                else case mh >>= toDirection of
                  Just (DAbsolute robotDir) -> Just $ DRelative $ entityDir `relativeTo` robotDir
                  _ -> Nothing -- This may happen if the robot is facing "down"
            val = VDir $ fromMaybe (DRelative DDown) $ do
              entLoc <- firstFound
              guard $ snd entLoc /= zero
              processDirection . nearestDirection . snd $ entLoc
        return $ Out val s k
      _ -> badConst
    Heading -> do
      mh <- use robotOrientation
      -- In general, (1) entities might not have an orientation, and
      -- (2) even if they do, orientation is a general vector, which
      -- might not correspond to a cardinal direction.  We could make
      -- 'heading' return a 'maybe dir' i.e. 'unit + dir', or return a
      -- vector of type 'int * int', but those would both be annoying
      -- for players in the vast majority of cases.  We rather choose
      -- to just return the direction 'down' in any case where we don't
      -- otherwise have anything reasonable to return.
      return $ Out (VDir (fromMaybe (DRelative DDown) $ mh >>= toDirection)) s k
    Time -> do
      t <- use ticks
      return $ Out (VInt t) s k
    Act -> case vs of
      [VDir d] -> do
        drillOut <- doDrill d
        return $ case drillOut of
          Out _ s' k' -> Out VUnit s' k'
          _ -> Out VUnit s k
      _ -> badConst
    Drill -> case vs of
      [VDir d] -> doDrill d
      _ -> badConst
    Use -> case vs of
      [VText deviceName, VDir d] -> do
        ins <- use equippedDevices
        equippedEntity <- ensureEquipped deviceName
        let verbPhrase = T.unwords ["use", deviceName, "on"]
        applyDevice ins verbPhrase d equippedEntity
      _ -> badConst
    Blocked -> do
      loc <- use robotLocation
      orient <- use robotOrientation
      let nextLoc = loc .+^ (orient ? zero)
      me <- entityAt nextLoc
      return $ Out (VBool (maybe False (`hasProperty` Unwalkable) me)) s k
    Scan -> case vs of
      [VDir d] -> do
        (_loc, me) <- lookInDirection d
        for_ me $ \e -> do
          robotInventory %= insertCount 0 e
          updateDiscoveredEntities e
          -- Flag the world for a redraw since scanning something may
          -- change the way it is drawn (if the base is doing the
          -- scanning)
          flagRedraw
        return $ Out (asValue me) s k
      _ -> badConst
    Knows -> case vs of
      [VText name] -> do
        inv <- use robotInventory
        ins <- use equippedDevices
        let allKnown = inv `E.union` ins
        let knows = case E.lookupByName name allKnown of
              [] -> False
              _ -> True
        return $ Out (VBool knows) s k
      _ -> badConst
    Upload -> case vs of
      [VRobot otherID] -> do
        -- Make sure the other robot exists and is close
        _other <- getRobotWithinTouch otherID

        -- Upload knowledge of everything in our inventory
        inv <- use robotInventory
        forM_ (elems inv) $ \(_, e) ->
          robotMap . at otherID . _Just . robotInventory %= insertCount 0 e

        -- Upload our log
        rlog <- use robotLog
        robotMap . at otherID . _Just . robotLog <>= rlog

        -- Flag the world for redraw since uploading may change the
        -- base's knowledge and hence how entities are drawn (if they
        -- go from unknown to known).
        flagRedraw

        return $ Out VUnit s k
      _ -> badConst
    Random -> case vs of
      [VInt hi] -> do
        n <- uniform (0, hi - 1)
        return $ Out (VInt n) s k
      _ -> badConst
    Atomic -> goAtomic
    Instant -> goAtomic
    As -> case vs of
      [VRobot rid, prog] -> do
        -- Get the named robot and current game state
        r <- robotWithID rid >>= (`isJustOrFail` ["There is no actor with ID", from (show rid)])
        g <- get @GameState

        -- Execute the given program *hypothetically*: i.e. in a fresh
        -- CESK machine, using *copies* of the current store, robot
        -- and game state.  We discard the state afterwards so any
        -- modifications made by prog do not persist.  Note we also
        -- set the copied robot to be a "system" robot so it is
        -- capable of executing any commands; the As command
        -- already requires "God" capability.
        v <-
          evalState @Robot (r & systemRobot .~ True) . evalState @GameState g $
            runCESK (Out prog s [FApp (VCApp Force []), FExec])

        -- Return the value returned by the hypothetical command.
        return $ Out v s k
      _ -> badConst
    RobotNamed -> case vs of
      [VText rname] -> do
        r <- robotWithName rname >>= (`isJustOrFail` ["There is no robot named", rname])
        return $ Out (asValue r) s k
      _ -> badConst
    RobotNumbered -> case vs of
      [VInt rid] -> do
        r <-
          robotWithID (fromIntegral rid)
            >>= (`isJustOrFail` ["There is no robot with number", from (show rid)])
        return $ Out (asValue r) s k
      _ -> badConst
    Say -> case vs of
      [VText msg] -> do
        isPrivileged <- isPrivilegedBot
        loc <- use robotLocation
        m <- traceLog Said msg -- current robot will inserted to robot set, so it needs the log
        emitMessage m
        let addLatestClosest rl = \case
              Seq.Empty -> Seq.singleton m
              es Seq.:|> e
                | e ^. leTime < m ^. leTime -> es |> e |> m
                | manhattan rl (e ^. leLocation) > manhattan rl (m ^. leLocation) -> es |> m
                | otherwise -> es |> e
        let addToRobotLog :: Has (State GameState) sgn m => Robot -> m ()
            addToRobotLog r = do
              r' <- execState r $ do
                hasLog <- hasCapability CLog
                hasListen <- hasCapability CListen
                loc' <- use robotLocation
                when (hasLog && hasListen) (robotLog %= addLatestClosest loc')
              addRobot r'
        robotsAround <-
          if isPrivileged
            then use $ robotMap . to IM.elems
            else gets $ robotsInArea loc hearingDistance
        mapM_ addToRobotLog robotsAround
        return $ Out VUnit s k
      _ -> badConst
    Listen -> do
      gs <- get @GameState
      loc <- use robotLocation
      rid <- use robotID
      isPrivileged <- isPrivilegedBot
      mq <- use messageQueue
      let isClose e = isPrivileged || messageIsFromNearby loc e
      let notMine e = rid /= e ^. leRobotID
      let limitLast = \case
            _s Seq.:|> l -> Just $ l ^. leText
            _ -> Nothing
      let mm = limitLast . Seq.filter (liftA2 (&&) notMine isClose) $ Seq.takeWhileR (messageIsRecent gs) mq
      return $
        maybe
          (In (TConst Listen) mempty s (FExec : k)) -- continue listening
          (\m -> Out (VText m) s k) -- return found message
          mm
    Log -> case vs of
      [VText msg] -> do
        void $ traceLog Logged msg
        return $ Out VUnit s k
      _ -> badConst
    View -> case vs of
      [VRobot rid] -> do
        -- Only the base can actually change the view in the UI.  Other robots can
        -- execute this command but it does nothing (at least for now).
        rn <- use robotID
        when (rn == 0) $
          robotWithID rid >>= \case
            -- If the robot does not exist...
            Nothing -> do
              cr <- use creativeMode
              ws <- use worldScrollable
              case cr || ws of
                -- If we are in creative mode or allowed to scroll, then we are allowed
                -- to learn that the robot doesn't exist.
                True -> throwError $ cmdExn c ["There is no actor with ID", from (show rid), "to view."]
                -- Otherwise, "unfocus" from any robot, which
                -- means the world view will turn to static.  The
                -- point is that there's no way to tell the difference
                -- between this situation and the situation where the
                -- robot exists but is too far away.
                False -> modify unfocus

            -- If it does exist, set it as the view center.
            Just _ -> viewCenterRule .= VCRobot rid

        return $ Out VUnit s k
      _ -> badConst
    Appear -> case vs of
      [VText app] -> do
        flagRedraw
        case into @String app of
          [dc] -> do
            robotDisplay . defaultChar .= dc
            robotDisplay . orientationMap .= M.empty
            return $ Out VUnit s k
          [dc, nc, ec, sc, wc] -> do
            robotDisplay . defaultChar .= dc
            robotDisplay . orientationMap . ix DNorth .= nc
            robotDisplay . orientationMap . ix DEast .= ec
            robotDisplay . orientationMap . ix DSouth .= sc
            robotDisplay . orientationMap . ix DWest .= wc
            return $ Out VUnit s k
          _other -> raise Appear [quote app, "is not a valid appearance string. 'appear' must be given a string with exactly 1 or 5 characters."]
      _ -> badConst
    Create -> case vs of
      [VText name] -> do
        em <- use entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name <> "."]

        robotInventory %= insert e
        updateDiscoveredEntities e

        return $ Out VUnit s k
      _ -> badConst
    Halt -> case vs of
      [VRobot targetID] -> do
        myID <- use robotID
        case myID == targetID of
          -- To halt ourselves, just return a cancelled CESK machine.
          -- It will be reinstalled as our current machine; then,
          -- based on the fact that our CESK machine is done we will
          -- be put to sleep and the REPL will be reset if we are the
          -- base robot.
          True -> return $ cancel $ Out VUnit s k
          False -> do
            -- Make sure the other robot exists and is close enough.
            target <- getRobotWithinTouch targetID
            -- Make sure either we are privileged, OR the target robot
            -- is NOT.  In other words unprivileged bots should not be
            -- able to halt privileged ones.
            omni <- isPrivilegedBot
            case omni || not (target ^. systemRobot) of
              True -> do
                -- Cancel its CESK machine, and put it to sleep.
                robotMap . at targetID . _Just . machine %= cancel
                sleepForever targetID
                return $ Out VUnit s k
              False -> throwError $ cmdExn c ["You are not authorized to halt that robot."]
      _ -> badConst
    Ishere -> case vs of
      [VText name] -> do
        loc <- use robotLocation
        me <- entityAt loc
        let here = maybe False (isEntityNamed name) me
        return $ Out (VBool here) s k
      _ -> badConst
    Isempty -> do
      loc <- use robotLocation
      me <- entityAt loc
      return $ Out (VBool (isNothing me)) s k
    Self -> do
      rid <- use robotID
      return $ Out (VRobot rid) s k
    Parent -> do
      mp <- use robotParentID
      rid <- use robotID
      return $ Out (VRobot (fromMaybe rid mp)) s k
    Base -> return $ Out (VRobot 0) s k
    Meet -> do
      loc <- use robotLocation
      rid <- use robotID
      g <- get @GameState
      let neighbor =
            find ((/= rid) . (^. robotID)) -- pick one other than ourself
              . sortOn (manhattan loc . (^. robotLocation)) -- prefer closer
              $ robotsInArea loc 1 g -- all robots within Manhattan distance 1
      return $ Out (asValue neighbor) s k
    MeetAll -> case vs of
      [f, b] -> do
        loc <- use robotLocation
        rid <- use robotID
        g <- get @GameState
        let neighborIDs = filter (/= rid) . map (^. robotID) $ robotsInArea loc 1 g
        return $ Out b s (FMeetAll f neighborIDs : k)
      _ -> badConst
    Whoami -> case vs of
      [] -> do
        name <- use robotName
        return $ Out (VText name) s k
      _ -> badConst
    Setname -> case vs of
      [VText name] -> do
        robotName .= name
        return $ Out VUnit s k
      _ -> badConst
    Force -> case vs of
      [VDelay t e] -> return $ In t e s k
      [VRef loc] ->
        -- To force a VRef, we look up the location in the store.
        case lookupCell loc s of
          -- If there's no cell at that location, it's a bug!  It
          -- shouldn't be possible to get a VRef to a non-existent
          -- location, since the only way VRefs get created is at the
          -- time we allocate a new cell.
          Nothing ->
            return $
              Up (Fatal $ T.append "Reference to unknown memory cell " (from (show loc))) s k
          -- If the location contains an unevaluated expression, it's
          -- time to evaluate it.  Set the cell to a 'Blackhole', push
          -- an 'FUpdate' frame so we remember to update the location
          -- to its value once we finish evaluating it, and focus on
          -- the expression.
          Just (E t e') -> return $ In t e' (setCell loc (Blackhole t e') s) (FUpdate loc : k)
          -- If the location contains a Blackhole, that means we are
          -- already currently in the middle of evaluating it, i.e. it
          -- depends on itself, so throw an 'InfiniteLoop' error.
          Just Blackhole {} -> return $ Up InfiniteLoop s k
          -- If the location already contains a value, just return it.
          Just (V v) -> return $ Out v s k
      -- If a force is applied to any other kind of value, just ignore it.
      -- This is needed because of the way we wrap all free variables in @force@
      -- in case they come from a @def@ which are always wrapped in @delay@.
      -- But binders (i.e. @x <- ...@) are also exported to the global context.
      [v] -> return $ Out v s k
      _ -> badConst
    If -> case vs of
      -- Use the boolean to pick the correct branch, and apply @force@ to it.
      [VBool b, thn, els] -> return $ Out (bool els thn b) s (FApp (VCApp Force []) : k)
      _ -> badConst
    Inl -> case vs of
      [v] -> return $ Out (VInj False v) s k
      _ -> badConst
    Inr -> case vs of
      [v] -> return $ Out (VInj True v) s k
      _ -> badConst
    Case -> case vs of
      [VInj side v, kl, kr] -> return $ Out v s (FApp (bool kl kr side) : k)
      _ -> badConst
    Fst -> case vs of
      [VPair v _] -> return $ Out v s k
      _ -> badConst
    Snd -> case vs of
      [VPair _ v] -> return $ Out v s k
      _ -> badConst
    Try -> case vs of
      [c1, c2] -> return $ Out c1 s (FApp (VCApp Force []) : FExec : FTry c2 : k)
      _ -> badConst
    Undefined -> return $ Up (User "undefined") s k
    Fail -> case vs of
      [VText msg] -> return $ Up (User msg) s k
      _ -> badConst
    Key -> case vs of
      [VText ktxt] -> case runParser parseKeyComboFull ktxt of
        Right kc -> return $ Out (VKey kc) s k
        Left _ -> return $ Up (CmdFailed Key (T.unwords ["Unknown key", quote ktxt]) Nothing) s k
      _ -> badConst
    InstallKeyHandler -> case vs of
      [VText hint, handler] -> do
        inputHandler .= Just (hint, handler)
        return $ Out VUnit s k
      _ -> badConst
    Reprogram -> case vs of
      [VRobot childRobotID, VDelay cmd e] -> do
        r <- get
        isPrivileged <- isPrivilegedBot

        -- check if robot exists
        childRobot <-
          robotWithID childRobotID
            >>= (`isJustOrFail` ["There is no actor with ID", from (show childRobotID) <> "."])

        -- check that current robot is not trying to reprogram self
        myID <- use robotID
        (childRobotID /= myID)
          `holdsOrFail` ["You cannot make a robot reprogram itself."]

        -- check if robot has completed executing it's current command
        _ <-
          finalValue (childRobot ^. machine)
            `isJustOrFail` ["You cannot reprogram a robot that is actively running a program."]

        -- check if childRobot is at the correct distance
        -- a robot can program adjacent robots
        -- privileged bots ignore distance checks
        loc <- use robotLocation
        (isPrivileged || (childRobot ^. robotLocation) `manhattan` loc <= 1)
          `holdsOrFail` ["You can only reprogram an adjacent robot."]

        -- Figure out if we can supply what the target robot requires,
        -- and if so, what is needed.
        (toEquip, toGive) <-
          checkRequirements
            (r ^. robotInventory)
            (childRobot ^. robotInventory)
            (childRobot ^. equippedDevices)
            cmd
            "The target robot"
            FixByObtain

        -- update other robot's CESK machine, environment and context
        -- the childRobot inherits the parent robot's environment
        -- and context which collectively mean all the variables
        -- declared in the parent robot
        robotMap . at childRobotID . _Just . machine .= In cmd e s [FExec]
        robotMap . at childRobotID . _Just . robotContext .= r ^. robotContext

        -- Provision the target robot with any required devices and
        -- inventory that are lacking.
        provisionChild childRobotID (fromList . S.toList $ toEquip) toGive

        -- Finally, re-activate the reprogrammed target robot.
        activateRobot childRobotID

        return $ Out VUnit s k
      _ -> badConst
    Build -> case vs of
      -- NOTE, pattern-matching on a VDelay here means we are
      -- /relying/ on the fact that 'Build' can only be given a
      -- /non-memoized/ delayed value.  If it were given a memoized
      -- delayed value we would see a VRef instead of a VDelay.  If
      -- and Try are generalized to handle any type of delayed value,
      -- but Build and Reprogram still assume they are given a VDelay
      -- and not a VRef.  In the future, if we enable memoized delays
      -- by default, or allow the user to explicitly request
      -- memoization via double braces or something similar, this will
      -- have to be generalized.  The difficulty is that we do a
      -- capability check on the delayed program at runtime, just
      -- before creating the newly built robot (see the call to
      -- 'requirements' below); but if we have a VRef instead of a
      -- VDelay, we may only be able to get a Value out of it instead
      -- of a Term as we currently do, and capability checking a Value
      -- is annoying and/or problematic.  One solution might be to
      -- annotate delayed expressions with their required capabilities
      -- at typechecking time, and carry those along so they flow to
      -- this point. Another solution would be to just bite the bullet
      -- and figure out how to do capability checking on Values (which
      -- would return the capabilities needed to *execute* them),
      -- hopefully without duplicating too much code.
      [VDelay cmd e] -> do
        r <- get @Robot
        pid <- use robotID

        (toEquip, toGive) <-
          checkRequirements (r ^. robotInventory) E.empty E.empty cmd "You" FixByObtain

        -- Pick a random display name.
        displayName <- randomName
        createdAt <- getNow

        -- Construct the new robot and add it to the world.
        parentCtx <- use robotContext
        newRobot <-
          addTRobot . (trobotContext .~ parentCtx) $
            mkRobot
              ()
              (Just pid)
              displayName
              ["A robot built by the robot named " <> r ^. robotName <> "."]
              (Just (r ^. robotLocation))
              ( ((r ^. robotOrientation) >>= \dir -> guard (dir /= zero) >> return dir)
                  ? north
              )
              defaultRobotDisplay
              (In cmd e s [FExec])
              []
              []
              False
              False
              createdAt

        -- Provision the new robot with the necessary devices and inventory.
        provisionChild (newRobot ^. robotID) (fromList . S.toList $ toEquip) toGive

        -- Flag the world for a redraw and return the name of the newly constructed robot.
        flagRedraw
        return $ Out (asValue newRobot) s k
      _ -> badConst
    Salvage -> case vs of
      [] -> do
        loc <- use robotLocation
        let okToSalvage r = (r ^. robotID /= 0) && (not . isActive $ r)
        mtarget <- gets (find okToSalvage . robotsAtLocation loc)
        case mtarget of
          Nothing -> return $ Out VUnit s k -- Nothing to salvage
          Just target -> do
            -- Copy the salvaged robot's equipped devices into its inventory, in preparation
            -- for transferring it.
            let salvageInventory = E.union (target ^. robotInventory) (target ^. equippedDevices)
            robotMap . at (target ^. robotID) . traverse . robotInventory .= salvageInventory

            let salvageItems = concatMap (\(n, e) -> replicate n (e ^. entityName)) (E.elems salvageInventory)
                numItems = length salvageItems

            -- Copy over the salvaged robot's log, if we have one
            inst <- use equippedDevices
            em <- use entityMap
            isPrivileged <- isPrivilegedBot
            logger <-
              lookupEntityName "logger" em
                `isJustOr` Fatal "While executing 'salvage': there's no such thing as a logger!?"
            when (isPrivileged || inst `E.contains` logger) $ robotLog <>= target ^. robotLog

            -- Immediately copy over any items the robot knows about
            -- but has 0 of
            let knownItems = map snd . filter ((== 0) . fst) . elems $ salvageInventory
            robotInventory %= \i -> foldr (insertCount 0) i knownItems

            -- Now reprogram the robot being salvaged to 'give' each
            -- item in its inventory to us, one at a time, then
            -- self-destruct at the end.  Make it a system robot so we
            -- don't have to worry about capabilities.
            robotMap . at (target ^. robotID) . traverse . systemRobot .= True

            ourID <- use @Robot robotID

            -- The program for the salvaged robot to run
            let giveInventory =
                  foldr (TBind Nothing . giveItem) (TConst Selfdestruct) salvageItems
                giveItem item = TApp (TApp (TConst Give) (TRobot ourID)) (TText item)

            -- Reprogram and activate the salvaged robot
            robotMap
              . at (target ^. robotID)
              . traverse
              . machine
              .= In giveInventory empty emptyStore [FExec]
            activateRobot (target ^. robotID)

            -- Now wait the right amount of time for it to finish.
            time <- use ticks
            return $ Waiting (time + fromIntegral numItems + 1) (Out VUnit s k)
      _ -> badConst
    -- run can take both types of text inputs
    -- with and without file extension as in
    -- "./path/to/file.sw" and "./path/to/file"
    Run -> case vs of
      [VText fileName] -> do
        let filePath = into @String fileName
        let e2m = fmap eitherToMaybe . runExceptT
        sData <- sendIO $ e2m $ getDataFileNameSafe Script filePath
        sDataSW <- sendIO $ e2m $ getDataFileNameSafe Script (filePath <> ".sw")
        mf <- sendIO $ mapM readFileMay $ [filePath, filePath <> ".sw"] <> catMaybes [sData, sDataSW]

        f <- msum mf `isJustOrFail` ["File not found:", fileName]

        mt <-
          processTerm (into @Text f) `isRightOr` \err ->
            cmdExn Run ["Error in", fileName, "\n", err]

        case mt of
          Nothing -> return $ Out VUnit s k
          Just t@(ProcessedTerm _ _ reqCtx) -> do
            -- Add the reqCtx from the ProcessedTerm to the current robot's defReqs.
            -- See #827 for an explanation of (1) why this is needed, (2) why
            -- it's slightly technically incorrect, and (3) why it is still way
            -- better than what we had before.
            robotContext . defReqs <>= reqCtx
            return $ initMachine' t empty s k
      _ -> badConst
    Not -> case vs of
      [VBool b] -> return $ Out (VBool (not b)) s k
      _ -> badConst
    Neg -> case vs of
      [VInt n] -> return $ Out (VInt (-n)) s k
      _ -> badConst
    Eq -> returnEvalCmp
    Neq -> returnEvalCmp
    Lt -> returnEvalCmp
    Gt -> returnEvalCmp
    Leq -> returnEvalCmp
    Geq -> returnEvalCmp
    And -> case vs of
      [VBool a, VBool b] -> return $ Out (VBool (a && b)) s k
      _ -> badConst
    Or -> case vs of
      [VBool a, VBool b] -> return $ Out (VBool (a || b)) s k
      _ -> badConst
    Add -> returnEvalArith
    Sub -> returnEvalArith
    Mul -> returnEvalArith
    Div -> returnEvalArith
    Exp -> returnEvalArith
    Format -> case vs of
      [v] -> return $ Out (VText (prettyValue v)) s k
      _ -> badConst
    Chars -> case vs of
      [VText t] -> return $ Out (VInt (fromIntegral $ T.length t)) s k
      _ -> badConst
    Split -> case vs of
      [VInt i, VText t] ->
        let p = T.splitAt (fromInteger i) t
            t2 = over both VText p
         in return $ Out (uncurry VPair t2) s k
      _ -> badConst
    Concat -> case vs of
      [VText v1, VText v2] -> return $ Out (VText (v1 <> v2)) s k
      _ -> badConst
    CharAt -> case vs of
      [VInt i, VText t]
        | i < 0 || i >= fromIntegral (T.length t) ->
            raise CharAt ["Index", prettyValue (VInt i), "out of bounds for length", from @String $ show (T.length t)]
        | otherwise -> return $ Out (VInt . fromIntegral . ord . T.index t . fromIntegral $ i) s k
      _ -> badConst
    ToChar -> case vs of
      [VInt i]
        | i < 0 || i > fromIntegral (ord (maxBound :: Char)) ->
            raise ToChar ["Value", prettyValue (VInt i), "is an invalid character code"]
        | otherwise ->
            return $ Out (VText . T.singleton . chr . fromIntegral $ i) s k
      _ -> badConst
    AppF ->
      let msg = "The operator '$' should only be a syntactic sugar and removed in elaboration:\n"
       in throwError . Fatal $ msg <> badConstMsg
 where
  doDrill d = do
    ins <- use equippedDevices

    let equippedDrills = extantElemsWithCapability CDrill ins
        -- Heuristic: choose the drill with the more elaborate name.
        -- E.g. "metal drill" vs. "drill"
        preferredDrill = listToMaybe $ sortOn (Down . T.length . (^. entityName)) equippedDrills

    tool <- preferredDrill `isJustOr` Fatal "Drill is required but not equipped?!"
    applyDevice ins "drill" d tool

  applyDevice ins verbPhrase d tool = do
    (nextLoc, nextE) <- getDrillTarget verbPhrase d
    inRs <- use recipesIn

    let recipes = filter drilling (recipesFor inRs nextE)
        drilling = any ((== tool) . snd) . view recipeRequirements

    not (null recipes) `holdsOrFail` ["There is no way to", verbPhrase, indefinite (nextE ^. entityName) <> "."]

    inv <- use robotInventory

    -- add the targeted entity so it can be consumed by the recipe
    let makeRecipe r = (,r) <$> make' (insert nextE inv, ins) r
    chosenRecipe <- weightedChoice (\((_, _), r) -> r ^. recipeWeight) (rights (map makeRecipe recipes))
    ((invTaken, outs), recipe) <-
      chosenRecipe
        `isJustOrFail` ["You don't have the ingredients to", verbPhrase, indefinite (nextE ^. entityName) <> "."]

    let (out, down) = L.partition ((`hasProperty` Portable) . snd) outs
    let learn = map (LearnEntity . snd) down
    let gain = map (uncurry AddEntity) out

    newEntity <- case down of
      [] -> pure Nothing
      [(1, de)] -> pure $ Just de
      _ -> throwError $ Fatal "Bad recipe:\n more than one unmovable entity produced."
    let changeWorld =
          ReplaceEntity
            { updatedLoc = nextLoc
            , originalEntity = nextE
            , newEntity = newEntity
            }

    -- take recipe inputs from inventory and add outputs after recipeTime
    robotInventory .= invTaken

    let cmdOutput = asValue $ snd <$> listToMaybe out
    finishCookingRecipe recipe cmdOutput [changeWorld] (learn <> gain)

  getDrillTarget verb d = do
    rname <- use robotName

    (nextLoc, nextME) <- lookInDirection d
    nextE <-
      nextME
        `isJustOrFail` ["There is nothing to", verb, directionText, "robot", rname <> "."]
    return (nextLoc, nextE)
   where
    directionText = case d of
      DRelative DDown -> "under"
      DRelative DForward -> "ahead of"
      DRelative DBack -> "behind"
      _ -> directionSyntax d <> " of"

  goAtomic :: HasRobotStepState sig m => m CESK
  goAtomic = case vs of
    -- To execute an atomic block, set the runningAtomic flag,
    -- push an FFinishAtomic frame so that we unset the flag when done, and
    -- proceed to execute the argument.
    [cmd] -> do
      runningAtomic .= True
      return $ Out cmd s (FExec : FFinishAtomic : k)
    _ -> badConst

  -- Case-insensitive matching on entity names
  isEntityNamed :: T.Text -> Entity -> Bool
  isEntityNamed n e = ((==) `on` T.toLower) (e ^. entityName) n

  badConst :: HasRobotStepState sig m => m a
  badConst = throwError $ Fatal badConstMsg

  badConstMsg :: Text
  badConstMsg =
    T.unlines
      [ "Bad application of execConst:"
      , T.pack (show c)
      , T.pack (show (reverse vs))
      , prettyText (Out (VCApp c (reverse vs)) s k)
      ]

  rectCells :: Integer -> Integer -> Integer -> Integer -> [V2 Int32]
  rectCells x1 y1 x2 y2 =
    rectCellsInt32
      (fromIntegral x1)
      (fromIntegral y1)
      (fromIntegral x2)
      (fromIntegral y2)

  rectCellsInt32 :: Int32 -> Int32 -> Int32 -> Int32 -> [V2 Int32]
  rectCellsInt32 x1 y1 x2 y2 = [V2 x y | x <- [xMin .. xMax], y <- [yMin .. yMax]]
   where
    (xMin, xMax) = sortPair (x1, x2)
    (yMin, yMax) = sortPair (y1, y2)

  findNearest ::
    HasRobotStepState sig m =>
    Text ->
    m (Maybe (Int32, V2 Int32))
  findNearest name = do
    loc <- use robotLocation
    findM (fmap (maybe False $ isEntityNamed name) . entityAt . (loc .+^) . snd) sortedLocs
   where
    sortedLocs :: [(Int32, V2 Int32)]
    sortedLocs = (0, zero) : concatMap genDiamondSides [1 .. maxSniffRange]

    -- Grow a list of locations in a diamond shape outward, such that the nearest cells
    -- are searched first by construction, rather than having to sort.
    genDiamondSides :: Int32 -> [(Int32, V2 Int32)]
    genDiamondSides diameter = concat [f diameter x | x <- [0 .. diameter]]
     where
      -- Adds a single cell to each of the four sides of the diamond
      f d x = map (d,) . take 4 . iterate perp $ V2 x (d - x)

  finishCookingRecipe ::
    HasRobotStepState sig m =>
    Recipe e ->
    Value ->
    [WorldUpdate Entity] ->
    [RobotUpdate] ->
    m CESK
  finishCookingRecipe r v wf rf =
    if remTime <= 0
      then do
        updateWorldAndRobots c wf rf
        return $ Out v s k
      else do
        time <- use ticks
        return . (if remTime <= 1 then id else Waiting (remTime + time)) $
          Out v s (FImmediate c wf rf : k)
   where
    remTime = r ^. recipeTime

  deriveHeading :: HasRobotStepState sig m => Direction -> m Heading
  deriveHeading d = do
    orient <- use robotOrientation
    when (isCardinal d) $ hasCapabilityFor COrient $ TDir d
    return $ applyTurn d $ orient ? zero

  lookInDirection :: HasRobotStepState sig m => Direction -> m (Location, Maybe Entity)
  lookInDirection d = do
    newHeading <- deriveHeading d
    loc <- use robotLocation
    let nextLoc = loc .+^ newHeading
    (nextLoc,) <$> entityAt nextLoc

  ensureEquipped :: HasRobotStepState sig m => Text -> m Entity
  ensureEquipped itemName = do
    inst <- use equippedDevices
    listToMaybe (lookupByName itemName inst)
      `isJustOrFail` ["You don't have a", indefinite itemName, "equipped."]

  ensureItem :: HasRobotStepState sig m => Text -> Text -> m Entity
  ensureItem itemName action = do
    -- First, make sure we know about the entity.
    inv <- use robotInventory
    inst <- use equippedDevices
    item <-
      asum (map (listToMaybe . lookupByName itemName) [inv, inst])
        `isJustOrFail` ["What is", indefinite itemName <> "?"]

    -- Next, check whether we have one.  If we don't, add a hint about
    -- 'create' in creative mode.
    creative <- use creativeMode
    let create l = l <> ["You can make one first with 'create \"" <> itemName <> "\"'." | creative]

    (E.lookup item inv > 0)
      `holdsOrFail` create ["You don't have", indefinite itemName, "to", action <> "."]

    return item

  -- Check the required devices and inventory for running the given
  -- command on a target robot.  This function is used in common by
  -- both 'Build' and 'Reprogram'.
  --
  -- It is given as inputs the parent robot inventory, the inventory
  -- and equipped devices of the child (these will be empty in the
  -- case of 'Build'), and the command to be run (along with a few
  -- inputs to configure any error messages to be generated).
  --
  -- Throws an exception if it's not possible to set up the child
  -- robot with the things it needs to execute the given program.
  -- Otherwise, returns a pair consisting of the set of devices to be
  -- equipped, and the inventory that should be transferred from
  -- parent to child.
  checkRequirements ::
    HasRobotStepState sig m =>
    Inventory ->
    Inventory ->
    Inventory ->
    Term ->
    Text ->
    IncapableFix ->
    m (Set Entity, Inventory)
  checkRequirements parentInventory childInventory childDevices cmd subject fixI = do
    currentContext <- use $ robotContext . defReqs
    em <- use entityMap
    creative <- use creativeMode
    let -- Note that _capCtx must be empty: at least at the
        -- moment, definitions are only allowed at the top level,
        -- so there can't be any inside the argument to build.
        -- (Though perhaps there is an argument that this ought to be
        -- relaxed specifically in the cases of 'Build' and 'Reprogram'.)
        -- See #349
        (R.Requirements (S.toList -> caps) (S.toList -> devNames) reqInvNames, _capCtx) = R.requirements currentContext cmd

    -- Check that all required device names exist (fail with
    -- an exception if not) and convert them to 'Entity' values.
    (devs :: [Entity]) <- forM devNames $ \devName ->
      E.lookupEntityName devName em `isJustOrFail` ["Unknown device required: " <> devName]

    -- Check that all required inventory entity names exist (fail with
    -- an exception if not) and convert them to 'Entity' values, with
    -- an associated count for each.
    (reqInv :: Inventory) <- fmap E.fromElems . forM (M.assocs reqInvNames) $ \(eName, n) ->
      (n,)
        <$> ( E.lookupEntityName eName em
                `isJustOrFail` ["Unknown entity required: " <> eName]
            )

    let -- List of possible devices per requirement.  For the
        -- requirements that stem from a required capability, we
        -- remember the capability alongside the possible devices, to
        -- help with later error message generation.
        possibleDevices :: [(Maybe Capability, [Entity])]
        possibleDevices =
          map (Just &&& (`deviceForCap` em)) caps -- Possible devices for capabilities
            ++ map ((Nothing,) . (: [])) devs -- Outright required devices

        -- A device is OK if it is available in the inventory of the
        -- parent robot, or already equipped in the child robot.
        deviceOK :: Entity -> Bool
        deviceOK d = parentInventory `E.contains` d || childDevices `E.contains` d

        -- Partition each list of possible devices into a set of
        -- available devices and a set of unavailable devices.
        -- There's a problem if some capability is required but no
        -- devices that provide it are available.  In that case we can
        -- print an error message, using the second set as a list of
        -- suggestions.
        partitionedDevices :: [(Set Entity, Set Entity)]
        partitionedDevices =
          map (Lens.over both S.fromList . L.partition deviceOK . snd) possibleDevices

        -- Devices equipped on the child, as a Set instead of an
        -- Inventory for convenience.
        alreadyEquipped :: Set Entity
        alreadyEquipped = S.fromList . map snd . E.elems $ childDevices

        -- Figure out what is still missing of the required inventory:
        -- the required inventory, less any inventory the child robot
        -- already has.
        missingChildInv = reqInv `E.difference` childInventory

    if creative
      then
        return
          ( -- In creative mode, just equip ALL the devices
            -- providing each required capability (because, why
            -- not?). But don't re-equip any that are already
            -- equipped.
            S.unions (map (S.fromList . snd) possibleDevices) `S.difference` alreadyEquipped
          , -- Conjure the necessary missing inventory out of thin
            -- air.
            missingChildInv
          )
      else do
        -- First, check that devices actually exist AT ALL to provide every
        -- required capability.  If not, we will generate an error message saying
        -- something like "missing capability X but no device yet provides it".
        let capsWithNoDevice = mapMaybe fst . filter (null . snd) $ possibleDevices
        null capsWithNoDevice
          `holdsOr` Incapable fixI (R.Requirements (S.fromList capsWithNoDevice) S.empty M.empty) cmd

        -- Now, ensure there is at least one device available to be
        -- equipped for each requirement.
        let missingDevices = map snd . filter (null . fst) $ partitionedDevices
        null missingDevices
          `holdsOrFail` ( singularSubjectVerb subject "do"
                            : "not have required devices, please"
                            : formatIncapableFix fixI <> ":"
                            : (("\n  - " <>) . formatDevices <$> missingDevices)
                        )

        let minimalEquipSet = smallHittingSet (filter (S.null . S.intersection alreadyEquipped) (map fst partitionedDevices))

            -- Check that we have enough in our inventory to cover the
            -- required devices PLUS what's missing from the child
            -- inventory.

            -- What do we need?
            neededParentInv =
              missingChildInv
                `E.union` (fromList . S.toList $ minimalEquipSet)

            -- What are we missing?
            missingParentInv = neededParentInv `E.difference` parentInventory
            missingMap =
              M.fromList
                . filter ((> 0) . snd)
                . map (swap . second (^. entityName))
                . E.elems
                $ missingParentInv

        -- If we're missing anything, throw an error
        E.isEmpty missingParentInv
          `holdsOr` Incapable fixI (R.Requirements S.empty S.empty missingMap) cmd

        return (minimalEquipSet, missingChildInv)

  destroyIfNotBase :: HasRobotStepState sig m => Maybe GameplayAchievement -> m ()
  destroyIfNotBase mAch = do
    rid <- use robotID
    holdsOrFailWithAchievement
      (rid /= 0)
      ["You consider destroying your base, but decide not to do it after all."]
      mAch
    selfDestruct .= True

  -- Make sure nothing is in the way. Note that system robots implicitly ignore
  -- and base throws on failure.
  checkMoveFailure :: HasRobotStepState sig m => Location -> m (Maybe MoveFailureDetails)
  checkMoveFailure nextLoc = do
    me <- entityAt nextLoc
    systemRob <- use systemRobot
    caps <- use robotCapabilities
    return $ do
      e <- me
      guard $ not systemRob
      go caps e
   where
    go caps e
      -- robots can not walk through walls
      | e `hasProperty` Unwalkable = Just $ MoveFailureDetails e PathBlocked
      -- robots drown if they walk over liquid without boat
      | e `hasProperty` Liquid && CFloat `S.notMember` caps =
          Just $ MoveFailureDetails e PathLiquid
      | otherwise = Nothing

  applyMoveFailureEffect ::
    HasRobotStepState sig m =>
    Maybe MoveFailureDetails ->
    MoveFailure ->
    m ()
  applyMoveFailureEffect maybeFailure MoveFailure {..} =
    case maybeFailure of
      Nothing -> return ()
      Just (MoveFailureDetails e failureMode) -> case failureMode of
        PathBlocked ->
          handleFailure
            failIfBlocked
            ["There is a", e ^. entityName, "in the way!"]
        PathLiquid ->
          handleFailure
            failIfDrown
            ["There is a dangerous liquid", e ^. entityName, "in the way!"]
   where
    handleFailure behavior message = case behavior of
      Destroy -> destroyIfNotBase Nothing
      ThrowExn -> throwError $ cmdExn c message
      IgnoreFail -> return ()

  -- Determine the move failure mode and apply the corresponding effect.
  checkMoveAhead :: HasRobotStepState sig m => Location -> MoveFailure -> m ()
  checkMoveAhead nextLoc failureHandlers = do
    maybeFailure <- checkMoveFailure nextLoc
    applyMoveFailureEffect maybeFailure failureHandlers

  getRobotWithinTouch :: HasRobotStepState sig m => RID -> m Robot
  getRobotWithinTouch rid = do
    cid <- use robotID
    if cid == rid
      then get @Robot
      else do
        mother <- robotWithID rid
        other <- mother `isJustOrFail` ["There is no robot with ID", from (show rid) <> "."]

        let otherLoc = other ^. robotLocation
        privileged <- isPrivilegedBot
        myLoc <- use robotLocation

        -- Make sure it is either in the same location or we do not care
        isNearbyOrExempt privileged myLoc otherLoc
          `holdsOrFail` ["The robot with ID", from (show rid), "is not close enough."]
        return other

  holdsOrFail :: (Has (Throw Exn) sig m) => Bool -> [Text] -> m ()
  holdsOrFail a ts = a `holdsOr` cmdExn c ts

  holdsOrFailWithAchievement :: (Has (Throw Exn) sig m) => Bool -> [Text] -> Maybe GameplayAchievement -> m ()
  holdsOrFailWithAchievement a ts mAch = case mAch of
    Nothing -> holdsOrFail a ts
    Just ach -> a `holdsOr` cmdExnWithAchievement c ts ach

  isJustOrFail :: (Has (Throw Exn) sig m) => Maybe a -> [Text] -> m a
  isJustOrFail a ts = a `isJustOr` cmdExn c ts

  returnEvalCmp = case vs of
    [v1, v2] -> (\b -> Out (VBool b) s k) <$> evalCmp c v1 v2
    _ -> badConst
  returnEvalArith = case vs of
    [VInt n1, VInt n2] -> (\r -> Out (VInt r) s k) <$> evalArith c n1 n2
    _ -> badConst

  -- Make sure the robot has the thing in its inventory
  hasInInventoryOrFail :: HasRobotStepState sig m => Text -> m Entity
  hasInInventoryOrFail eName = do
    inv <- use robotInventory
    e <-
      listToMaybe (lookupByName eName inv)
        `isJustOrFail` ["What is", indefinite eName <> "?"]
    let cmd = T.toLower . T.pack . show $ c
    (E.lookup e inv > 0)
      `holdsOrFail` ["You don't have", indefinite eName, "to", cmd <> "."]
    return e

  -- The code for grab and harvest is almost identical, hence factored
  -- out here.
  doGrab :: (HasRobotStepState sig m, Has (Lift IO) sig m) => GrabbingCmd -> m CESK
  doGrab cmd = do
    let verb = verbGrabbingCmd cmd
        verbed = verbedGrabbingCmd cmd

    -- Ensure there is an entity here.
    loc <- use robotLocation
    e <-
      entityAt loc
        >>= (`isJustOrFail` ["There is nothing here to", verb <> "."])

    -- Ensure it can be picked up.
    omni <- isPrivilegedBot
    (omni || e `hasProperty` Portable)
      `holdsOrFail` ["The", e ^. entityName, "here can't be", verbed <> "."]

    -- Remove the entity from the world.
    updateEntityAt loc (const Nothing)
    flagRedraw

    -- Immediately regenerate entities with 'infinite' property.
    when (e `hasProperty` Infinite) $
      updateEntityAt loc (const (Just e))

    -- Possibly regrow the entity, if it is growable and the 'harvest'
    -- command was used.
    when ((e `hasProperty` Growable) && cmd == Harvest') $ do
      let GrowthTime (minT, maxT) = (e ^. entityGrowth) ? defaultGrowthTime

      createdAt <- getNow

      -- Grow a new entity from a seed.
      addSeedBot e (minT, maxT) loc createdAt

    -- Add the picked up item to the robot's inventory.  If the
    -- entity yields something different, add that instead.
    let yieldName = e ^. entityYields
    e' <- case yieldName of
      Nothing -> return e
      Just n -> fromMaybe e <$> uses entityMap (lookupEntityName n)

    robotInventory %= insert e'
    updateDiscoveredEntities e'

    -- Return the name of the item obtained.
    return $ Out (VText (e' ^. entityName)) s k

------------------------------------------------------------
-- The "watch" command
------------------------------------------------------------

addWatchedLocation ::
  HasRobotStepState sig m =>
  Location ->
  m ()
addWatchedLocation loc = do
  rid <- use robotID
  robotsWatching %= M.insertWith (<>) loc (S.singleton rid)

-- | Clear watches that are out of range
purgeFarAwayWatches ::
  HasRobotStepState sig m => m ()
purgeFarAwayWatches = do
  privileged <- isPrivilegedBot
  myLoc <- use robotLocation
  rid <- use robotID

  let isNearby = isNearbyOrExempt privileged myLoc
      f loc =
        if not $ isNearby loc
          then S.delete rid
          else id

  robotsWatching %= M.filter (not . null) . M.mapWithKey f

------------------------------------------------------------
-- Some utility functions
------------------------------------------------------------

-- | Exempts the robot from various command constraints
-- when it is either a system robot or playing in creative mode
isPrivilegedBot :: (Has (State GameState) sig m, Has (State Robot) sig m) => m Bool
isPrivilegedBot = (||) <$> use systemRobot <*> use creativeMode

-- | Requires that the target location is within one cell.
-- Requirement is waived if the bot is privileged.
isNearbyOrExempt :: Bool -> Location -> Location -> Bool
isNearbyOrExempt privileged myLoc otherLoc =
  privileged || otherLoc `manhattan` myLoc <= 1

grantAchievement ::
  (Has (State GameState) sig m, Has (Lift IO) sig m) =>
  GameplayAchievement ->
  m ()
grantAchievement a = do
  currentTime <- sendIO getZonedTime
  scenarioPath <- use currentScenarioPath
  gameAchievements
    %= M.insertWith
      (<>)
      a
      (Attainment (GameplayAchievement a) scenarioPath currentTime)

data MoveFailureMode = PathBlocked | PathLiquid
data MoveFailureDetails = MoveFailureDetails Entity MoveFailureMode

-- | How to handle failure, for example when moving to blocked location
data RobotFailure = ThrowExn | Destroy | IgnoreFail

-- | How to handle failure when moving/teleporting to a location.
data MoveFailure = MoveFailure
  { failIfBlocked :: RobotFailure
  , failIfDrown :: RobotFailure
  }

data GrabbingCmd = Grab' | Harvest' | Swap' | Push' deriving (Eq, Show)

verbGrabbingCmd :: GrabbingCmd -> Text
verbGrabbingCmd = \case
  Harvest' -> "harvest"
  Grab' -> "grab"
  Swap' -> "swap"
  Push' -> "push"

verbedGrabbingCmd :: GrabbingCmd -> Text
verbedGrabbingCmd = \case
  Harvest' -> "harvested"
  Grab' -> "grabbed"
  Swap' -> "swapped"
  Push' -> "pushed"

-- | Format a set of suggested devices for use in an error message,
--   in the format @device1 or device2 or ... or deviceN@.
formatDevices :: Set Entity -> Text
formatDevices = T.intercalate " or " . map (^. entityName) . S.toList

-- | Give some entities from a parent robot (the robot represented by
--   the ambient @State Robot@ effect) to a child robot (represented
--   by the given 'RID') as part of a 'Build' or 'Reprogram' command.
--   The first 'Inventory' is devices to be equipped, and the second
--   is entities to be transferred.
--
--   In classic mode, the entities will be /transferred/ (that is,
--   removed from the parent robot's inventory); in creative mode, the
--   entities will be copied/created, that is, no entities will be
--   removed from the parent robot.
provisionChild ::
  (HasRobotStepState sig m) =>
  RID ->
  Inventory ->
  Inventory ->
  m ()
provisionChild childID toEquip toGive = do
  -- Equip and give devices to child
  robotMap . ix childID . equippedDevices %= E.union toEquip
  robotMap . ix childID . robotInventory %= E.union toGive

  -- Delete all items from parent in classic mode
  creative <- use creativeMode
  unless creative $
    robotInventory %= (`E.difference` (toEquip `E.union` toGive))

-- | Update the location of a robot, and simultaneously update the
--   'robotsByLocation' map, so we can always look up robots by
--   location.  This should be the /only/ way to update the location
--   of a robot.
updateRobotLocation ::
  (HasRobotStepState sig m) =>
  Location ->
  Location ->
  m ()
updateRobotLocation oldLoc newLoc
  | oldLoc == newLoc = return ()
  | otherwise = do
      rid <- use robotID
      robotsByLocation . at oldLoc %= deleteOne rid
      robotsByLocation . at newLoc . non Empty %= IS.insert rid
      modify (unsafeSetRobotLocation newLoc)
      flagRedraw
 where
  -- Make sure empty sets don't hang around in the
  -- robotsByLocation map.  We don't want a key with an
  -- empty set at every location any robot has ever
  -- visited!
  deleteOne _ Nothing = Nothing
  deleteOne x (Just s)
    | IS.null s' = Nothing
    | otherwise = Just s'
   where
    s' = IS.delete x s

-- | Execute a stateful action on a target robot --- whether the
--   current one or another.
onTarget ::
  HasRobotStepState sig m =>
  RID ->
  (forall sig' m'. (HasRobotStepState sig' m') => m' ()) ->
  m ()
onTarget rid act = do
  myID <- use robotID
  case myID == rid of
    True -> act
    False -> do
      mtgt <- use (robotMap . at rid)
      case mtgt of
        Nothing -> return ()
        Just tgt -> do
          tgt' <- execState @Robot tgt act
          if tgt' ^. selfDestruct
            then deleteRobot rid
            else robotMap . ix rid .= tgt'

------------------------------------------------------------
-- Comparison
------------------------------------------------------------

-- | Evaluate the application of a comparison operator.  Returns
--   @Nothing@ if the application does not make sense.
evalCmp :: Has (Throw Exn) sig m => Const -> Value -> Value -> m Bool
evalCmp c v1 v2 = decideCmp c $ compareValues v1 v2
 where
  decideCmp = \case
    Eq -> fmap (== EQ)
    Neq -> fmap (/= EQ)
    Lt -> fmap (== LT)
    Gt -> fmap (== GT)
    Leq -> fmap (/= GT)
    Geq -> fmap (/= LT)
    _ -> const . throwError . Fatal . T.append "evalCmp called on bad constant " . from $ show c

-- | Compare two values, returning an 'Ordering' if they can be
--   compared, or @Nothing@ if they cannot.
compareValues :: Has (Throw Exn) sig m => Value -> Value -> m Ordering
compareValues v1 = case v1 of
  VUnit -> \case VUnit -> return EQ; v2 -> incompatCmp VUnit v2
  VInt n1 -> \case VInt n2 -> return (compare n1 n2); v2 -> incompatCmp v1 v2
  VText t1 -> \case VText t2 -> return (compare t1 t2); v2 -> incompatCmp v1 v2
  VDir d1 -> \case VDir d2 -> return (compare d1 d2); v2 -> incompatCmp v1 v2
  VBool b1 -> \case VBool b2 -> return (compare b1 b2); v2 -> incompatCmp v1 v2
  VRobot r1 -> \case VRobot r2 -> return (compare r1 r2); v2 -> incompatCmp v1 v2
  VInj s1 v1' -> \case
    VInj s2 v2' ->
      case compare s1 s2 of
        EQ -> compareValues v1' v2'
        o -> return o
    v2 -> incompatCmp v1 v2
  VPair v11 v12 -> \case
    VPair v21 v22 ->
      (<>) <$> compareValues v11 v21 <*> compareValues v12 v22
    v2 -> incompatCmp v1 v2
  VRcd m1 -> \case
    VRcd m2 -> mconcat <$> (zipWithM compareValues `on` M.elems) m1 m2
    v2 -> incompatCmp v1 v2
  VKey kc1 -> \case
    VKey kc2 -> return (compare kc1 kc2)
    v2 -> incompatCmp v1 v2
  VClo {} -> incomparable v1
  VCApp {} -> incomparable v1
  VDef {} -> incomparable v1
  VResult {} -> incomparable v1
  VBind {} -> incomparable v1
  VDelay {} -> incomparable v1
  VRef {} -> incomparable v1
  VRequirements {} -> incomparable v1

-- | Values with different types were compared; this should not be
--   possible since the type system should catch it.
incompatCmp :: Has (Throw Exn) sig m => Value -> Value -> m a
incompatCmp v1 v2 =
  throwError $
    Fatal $
      T.unwords ["Incompatible comparison of ", prettyValue v1, "and", prettyValue v2]

-- | Values were compared of a type which cannot be compared
--   (e.g. functions, etc.).
incomparable :: Has (Throw Exn) sig m => Value -> Value -> m a
incomparable v1 v2 =
  throwError $
    cmdExn
      Lt
      ["Comparison is undefined for ", prettyValue v1, "and", prettyValue v2]

------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------

-- | Evaluate the application of an arithmetic operator, returning
--   an exception in the case of a failing operation, or in case we
--   incorrectly use it on a bad 'Const' in the library.
evalArith :: Has (Throw Exn) sig m => Const -> Integer -> Integer -> m Integer
evalArith = \case
  Add -> ok (+)
  Sub -> ok (-)
  Mul -> ok (*)
  Div -> safeDiv
  Exp -> safeExp
  c -> \_ _ -> throwError $ Fatal $ T.append "evalArith called on bad constant " (from (show c))
 where
  ok f x y = return $ f x y

-- | Perform an integer division, but return @Nothing@ for division by
--   zero.
safeDiv :: Has (Throw Exn) sig m => Integer -> Integer -> m Integer
safeDiv _ 0 = throwError $ cmdExn Div $ pure "Division by zero"
safeDiv a b = return $ a `div` b

-- | Perform exponentiation, but return @Nothing@ if the power is negative.
safeExp :: Has (Throw Exn) sig m => Integer -> Integer -> m Integer
safeExp a b
  | b < 0 = throwError $ cmdExn Exp $ pure "Negative exponent"
  | otherwise = return $ a ^ b

------------------------------------------------------------
-- Updating discovered entities, recipes, and commands
------------------------------------------------------------

-- | Update the global list of discovered entities, and check for new recipes.
updateDiscoveredEntities :: (HasRobotStepState sig m) => Entity -> m ()
updateDiscoveredEntities e = do
  allDiscovered <- use allDiscoveredEntities
  if E.contains0plus e allDiscovered
    then pure ()
    else do
      let newAllDiscovered = E.insertCount 1 e allDiscovered
      updateAvailableRecipes (newAllDiscovered, newAllDiscovered) e
      updateAvailableCommands e
      allDiscoveredEntities .= newAllDiscovered

-- | Update the availableRecipes list.
-- This implementation is not efficient:
-- * Every time we discover a new entity, we iterate through the entire list of recipes to see which ones we can make.
--   Trying to do something more clever seems like it would definitely be a case of premature optimization.
--   One doesn't discover new entities all that often.
-- * For each usable recipe, we do a linear search through the list of known recipes to see if we already know it.
--   This is a little more troubling, since it's quadratic in the number of recipes.
--   But it probably doesn't really make that much difference until we get up to thousands of recipes.
updateAvailableRecipes :: Has (State GameState) sig m => (Inventory, Inventory) -> Entity -> m ()
updateAvailableRecipes invs e = do
  allInRecipes <- use recipesIn
  let entityRecipes = recipesFor allInRecipes e
      usableRecipes = filter (knowsIngredientsFor invs) entityRecipes
  knownRecipes <- use (availableRecipes . notificationsContent)
  let newRecipes = filter (`notElem` knownRecipes) usableRecipes
      newCount = length newRecipes
  availableRecipes %= mappend (Notifications newCount newRecipes)
  updateAvailableCommands e

updateAvailableCommands :: Has (State GameState) sig m => Entity -> m ()
updateAvailableCommands e = do
  let newCaps = e ^. entityCapabilities
      keepConsts = \case
        Just cap -> cap `S.member` newCaps
        Nothing -> False
      entityConsts = filter (keepConsts . constCaps) allConst
  knownCommands <- use (availableCommands . notificationsContent)
  let newCommands = filter (`notElem` knownCommands) entityConsts
      newCount = length newCommands
  availableCommands %= mappend (Notifications newCount newCommands)
