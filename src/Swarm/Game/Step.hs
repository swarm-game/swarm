{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Swarm.Game.Step
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Facilities for stepping the robot CESK machines, /i.e./ the actual
-- interpreter for the Swarm language.
module Swarm.Game.Step where

import Control.Lens as Lens hiding (Const, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Control.Monad (forM, forM_, guard, msum, unless, when)
import Data.Array (bounds, (!))
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Either (partitionEithers, rights)
import Data.Foldable (traverse_)
import Data.Functor.Const qualified as F
import Data.Int (Int64)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List (find)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Linear (V2 (..), zero, (^+^))
import System.Clock (TimeSpec)
import System.Clock qualified
import System.Random (UniformRange, uniformR)
import Witch (From (from), into)
import Prelude hiding (lookup)

import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Exception
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Value
import Swarm.Game.World qualified as W
import Swarm.Language.Capability
import Swarm.Language.Context hiding (delete)
import Swarm.Language.Pipeline
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax
import Swarm.Util

import Control.Carrier.Error.Either (runError)
import Control.Carrier.State.Lazy
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Data.Containers.ListUtils (nubOrd)
import Data.Functor (void)

-- | The main function to do one game tick.  The only reason we need
--   @IO@ is so that robots can run programs loaded from files, via
--   the 'Run' command; but eventually I want to get rid of that
--   command and have a library of modules that you can create, edit,
--   and run all from within the UI (the library could also be loaded
--   from a file when the whole program starts up).
gameTick :: (Has (State GameState) sig m, Has (Lift IO) sig m) => m ()
gameTick = do
  wakeUpRobotsDoneSleeping
  robotNames <- use activeRobots
  forM_ (IS.toList robotNames) $ \rn -> do
    mr <- uses robotMap (IM.lookup rn)
    case mr of
      Nothing -> return ()
      Just curRobot -> do
        curRobot' <- tickRobot curRobot
        if curRobot' ^. selfDestruct
          then deleteRobot rn
          else do
            robotMap %= IM.insert rn curRobot'
            let oldLoc = curRobot ^. robotLocation
                newLoc = curRobot' ^. robotLocation

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

            when (newLoc /= oldLoc) $ do
              robotsByLocation . at oldLoc %= deleteOne rn
              robotsByLocation . at newLoc . non Empty %= IS.insert rn
            time <- use ticks
            case waitingUntil curRobot' of
              Just wakeUpTime
                -- if w=2 t=1 then we do not needlessly put robot to waiting queue
                | wakeUpTime - 2 <= time -> return ()
                | otherwise -> sleepUntil rn wakeUpTime
              Nothing ->
                unless (isActive curRobot') (sleepForever rn)

  -- See if the base is finished with a computation, and if so, record
  -- the result in the game state so it can be displayed by the REPL;
  -- also save the current store into the robotContext so we can
  -- restore it the next time we start a computation.
  mr <- use (robotMap . at 0)
  case mr of
    Just r -> do
      res <- use replStatus
      case res of
        REPLWorking ty Nothing -> case getResult r of
          Just (v, s) -> do
            replStatus .= REPLWorking ty (Just v)
            robotMap . ix 0 . robotContext . defStore .= s
          Nothing -> return ()
        _otherREPLStatus -> return ()
    Nothing -> return ()

  -- Possibly update the view center.
  modify recalcViewCenter

  -- Possibly see if the winning condition for challenge mode is met.
  wc <- use winCondition
  case wc of
    WinCondition t -> do
      g <- get @GameState

      -- Execute the win condition check *hypothetically*: i.e. in a
      -- fresh CESK machine, using a copy of the current game state.
      v <- runThrow @Exn . evalState @GameState g $ evalPT t
      case v of
        Left _exn -> return () -- XXX
        Right (VBool True) -> winCondition .= Won False
        _ -> return ()
    _ -> return ()

  -- Advance the game time by one.
  ticks += 1

evalPT ::
  (Has (Lift IO) sig m, Has (Throw Exn) sig m, Has (State GameState) sig m) =>
  ProcessedTerm ->
  m Value
evalPT t = evaluateCESK (initMachine t empty emptyStore)

getNow :: Has (Lift IO) sig m => m TimeSpec
getNow = sendIO $ System.Clock.getTime System.Clock.Monotonic

evaluateCESK ::
  (Has (Lift IO) sig m, Has (Throw Exn) sig m, Has (State GameState) sig m) =>
  CESK ->
  m Value
evaluateCESK cesk = do
  createdAt <- getNow
  -- Use ID (-1) so it won't conflict with any robots currently in the robot map.
  let r = mkRobot (Identity (-1)) Nothing "" [] zero zero defaultRobotDisplay cesk [] [] True createdAt
  addRobot r -- Add the robot to the robot map, so it can look itself up if needed
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
entityAt :: (Has (State GameState) sig m) => V2 Int64 -> m (Maybe Entity)
entityAt loc = zoomWorld (W.lookupEntityM @Int (W.locToCoords loc))

-- | Modify the entity (if any) at a given location.
updateEntityAt ::
  (Has (State GameState) sig m) => V2 Int64 -> (Maybe Entity -> Maybe Entity) -> m ()
updateEntityAt loc upd = zoomWorld (W.updateM @Int (W.locToCoords loc) upd)

-- | Get the robot with a given ID.
robotWithID :: (Has (State GameState) sig m) => RID -> m (Maybe Robot)
robotWithID rid = use (robotMap . at rid)

-- | Get the robot with a given name.
robotWithName :: (Has (State GameState) sig m) => Text -> m (Maybe Robot)
robotWithName rname = use (robotMap . to IM.elems . to (find $ \r -> r ^. robotName == rname))

-- | Manhattan distance between world locations.
manhattan :: V2 Int64 -> V2 Int64 -> Int64
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

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

-- | For debugging only. Print some text via the robot's log.
traceLog :: (Has (State GameState) sig m, Has (State Robot) sig m) => Text -> m ()
traceLog msg = do
  rn <- use robotName
  time <- use ticks
  robotLog %= (Seq.|> LogEntry msg rn time)

-- | For debugging only. Print a showable value via the robot's log.
traceLogShow :: (Has (State GameState) sig m, Has (State Robot) sig m, Show a) => a -> m ()
traceLogShow = traceLog . from . show

------------------------------------------------------------
-- Exceptions and validation
------------------------------------------------------------

-- | Ensure that a robot is capable of executing a certain constant
--   (either because it has a device which gives it that capability,
--   or it is a system robot, or we are in creative mode).
ensureCanExecute :: (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Const -> m ()
ensureCanExecute c = case constCaps c of
  Nothing -> pure ()
  Just cap -> do
    creative <- use creativeMode
    sys <- use systemRobot
    robotCaps <- use robotCapabilities
    let hasCaps = cap `S.member` robotCaps
    (sys || creative || hasCaps)
      `holdsOr` Incapable FixByInstall (R.singletonCap cap) (TConst c)

-- | Test whether the current robot has a given capability (either
--   because it has a device which gives it that capability, or it is a
--   system robot, or we are in creative mode).
hasCapability :: (Has (State Robot) sig m, Has (State GameState) sig m) => Capability -> m Bool
hasCapability cap = do
  creative <- use creativeMode
  sys <- use systemRobot
  caps <- use robotCapabilities
  return (sys || creative || cap `S.member` caps)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityFor ::
  (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Capability -> Term -> m ()
hasCapabilityFor cap term = do
  h <- hasCapability cap
  h `holdsOr` Incapable FixByInstall (R.singletonCap cap) term

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts)

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
  return $ r' & machine .~ cesk'

-- | The main CESK machine workhorse.  Given a robot, look at its CESK
--   machine state and figure out a single next step.
stepCESK :: (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m) => CESK -> m CESK
stepCESK cesk = case cesk of
  -- (sendIO $ appendFile "out.txt" (prettyCESK cesk)) >>

  ------------------------------------------------------------
  -- Evaluation

  -- We wake up robots whose wake-up time has been reached. If it hasn't yet
  -- then stepCESK is a no-op.
  Waiting wakeupTime cesk' -> do
    time <- use ticks
    if wakeupTime <= time
      then stepCESK cesk'
      else return cesk
  Out v s (FImmediate wf rf : k) -> do
    wc <- worldUpdate wf <$> use world
    case wc of
      Left exn -> return $ Up exn s k
      Right wo -> do
        robotInventory %= robotUpdateInventory rf
        world .= wo
        needsRedraw .= True
        stepCESK (Out v s k)

  -- Now some straightforward cases.  These all immediately turn
  -- into values.
  In TUnit _ s k -> return $ Out VUnit s k
  In (TDir d) _ s k -> return $ Out (VDir d) s k
  In (TInt n) _ s k -> return $ Out (VInt n) s k
  In (TString str) _ s k -> return $ Out (VString str) s k
  In (TBool b) _ s k -> return $ Out (VBool b) s k
  -- There should not be any antiquoted variables left at this point.
  In (TAntiString v) _ s k ->
    return $ Up (Fatal (T.append "Antiquoted variable found at runtime: $str:" v)) s k
  In (TAntiInt v) _ s k ->
    return $ Up (Fatal (T.append "Antiquoted variable found at runtime: $int:" v)) s k
  -- Require and requireDevice just turn into no-ops.
  In (TRequireDevice {}) e s k -> return $ In (TConst Noop) e s k
  In (TRequire {}) e s k -> return $ In (TConst Noop) e s k
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
  -- If an exception rises all the way to the top level without being
  -- handled, turn it into an error message via the 'log' command.

  -- HOWEVER, we have to make sure to check that the robot has the
  -- 'log' capability, and silently discard the message otherwise.  If
  -- we didn't, trying to exceute the Log command would generate
  -- another exception, which will be logged, which would generate an
  -- exception, ... etc.
  --
  -- Notice how we call resetBlackholes on the store, so that any
  -- cells which were in the middle of being evaluated will be reset.
  Up exn s [] -> do
    let s' = resetBlackholes s
    h <- hasCapability CLog
    em <- use entityMap
    case h of
      True -> return $ In (TApp (TConst Log) (TString (formatExn em exn))) empty s' [FExec]
      False -> return $ Out VUnit s' []
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
            , from (prettyCESK cesk)
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
addSeedBot :: Has (State GameState) sig m => Entity -> (Integer, Integer) -> V2 Int64 -> TimeSpec -> m ()
addSeedBot e (minT, maxT) loc ts =
  void $
    addURobot $
      mkRobot
        (F.Const ())
        Nothing
        "seed"
        ["A growing seed."]
        loc
        (V2 0 0)
        ( defaultEntityDisplay '.'
            & displayAttr .~ (e ^. entityDisplay . displayAttr)
            & displayPriority .~ 0
        )
        (initMachine (seedProgram minT (maxT - minT) (e ^. entityName)) empty emptyStore)
        []
        [(1, e)]
        True
        ts

-- | Interpret the execution (or evaluation) of a constant application
--   to some values.
execConst ::
  (Has (State GameState) sig m, Has (State Robot) sig m, Has (Error Exn) sig m, Has (Lift IO) sig m) =>
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
        return $ Waiting (time + d) (Out VUnit s k)
      _ -> badConst
    Selfdestruct -> do
      destroyIfNotBase
      flagRedraw
      return $ Out VUnit s k
    Move -> do
      loc <- use robotLocation
      orient <- use robotOrientation
      let nextLoc = loc ^+^ (orient ? zero)
      me <- entityAt nextLoc

      -- Make sure nothing is in the way.
      case me of
        Nothing -> return ()
        Just e -> do
          (not . (`hasProperty` Unwalkable)) e
            `holdsOrFail` ["There is a", e ^. entityName, "in the way!"]

          -- Robots drown if they walk over liquid
          caps <- use robotCapabilities
          when
            (e `hasProperty` Liquid && CFloat `S.notMember` caps)
            destroyIfNotBase

      robotLocation .= nextLoc
      flagRedraw
      return $ Out VUnit s k
    Teleport -> case vs of
      [VRobot rid, VPair (VInt x) (VInt y)] -> do
        myID <- use robotID
        -- Make sure the other robot exists and is close
        target <- getRobotWithinTouch rid
        -- either change current robot or one in robot map
        let (.==) l a = if myID == rid then l .= a else robotMap . ix rid . l .= a
        let nextLoc = V2 (fromIntegral x) (fromIntegral y)
        let caps = target ^. robotCapabilities

        me <- entityAt nextLoc
        -- Make sure nothing is in the way.
        case me of
          Nothing -> return ()
          Just e -> do
            let unwalkable = e `hasProperty` Unwalkable
            let drowning = e `hasProperty` Liquid && CFloat `S.notMember` caps
            when (unwalkable || drowning) $ do
              d <- isDestroyable target
              when d $ selfDestruct .== True

        robotLocation .== nextLoc
        flagRedraw
        return $ Out VUnit s k
      _ -> badConst
    Grab -> doGrab False
    Harvest -> doGrab True
    Turn -> case vs of
      [VDir d] -> do
        when (isCardinal d) $ hasCapabilityFor COrient (TDir d)
        robotOrientation . _Just %= applyTurn d
        flagRedraw
        return $ Out VUnit s k
      _ -> badConst
    Place -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        loc <- use robotLocation

        -- Make sure there's nothing already here
        nothingHere <- isNothing <$> entityAt loc
        nothingHere `holdsOrFail` ["There is already an entity here."]

        -- Make sure the robot has the thing in its inventory
        e <-
          listToMaybe (lookupByName name inv)
            `isJustOrFail` ["What is", indefinite name <> "?"]

        (E.lookup e inv > 0)
          `holdsOrFail` ["You don't have", indefinite name, "to place."]

        -- Place the entity and remove it from the inventory
        updateEntityAt loc (const (Just e))
        robotInventory %= delete e

        flagRedraw
        return $ Out VUnit s k
      _ -> badConst
    Give -> case vs of
      [VRobot otherID, VString itemName] -> do
        -- Make sure the other robot exists and is close
        _other <- getRobotWithinTouch otherID

        -- Make sure we have the required item
        inv <- use robotInventory
        item <-
          (listToMaybe . lookupByName itemName $ inv)
            `isJustOrFail` ["What is", indefinite itemName <> "?"]

        (E.lookup item inv > 0)
          `holdsOrFail` ["You don't have", indefinite itemName, "to give."]

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
    Install -> case vs of
      [VRobot otherID, VString itemName] -> do
        -- Make sure the other robot exists and is close
        _other <- getRobotWithinTouch otherID

        -- Make sure we have the required item
        inv <- use robotInventory
        item <-
          (listToMaybe . lookupByName itemName $ inv)
            `isJustOrFail` ["What is", indefinite itemName <> "?"]

        (E.lookup item inv > 0)
          `holdsOrFail` ["You don't have", indefinite itemName, "to install."]

        myID <- use robotID
        focusedID <- use focusedRobotID
        case otherID == myID of
          -- We have to special case installing something on ourselves
          -- for the same reason as Give.
          True -> do
            -- Don't do anything if the robot already has the device.
            already <- use (installedDevices . to (`E.contains` item))
            unless already $ do
              installedDevices %= insert item
              robotInventory %= delete item

              -- Flag the UI for a redraw if we are currently showing our inventory
              when (focusedID == myID) flagRedraw
          False -> do
            let otherDevices = robotMap . at otherID . _Just . installedDevices
            already <- use $ pre (otherDevices . to (`E.contains` item))
            unless (already == Just True) $ do
              robotMap . at otherID . _Just . installedDevices %= insert item
              robotInventory %= delete item

              -- Flag the UI for a redraw if we are currently showing
              -- either robot's inventory
              when (focusedID == myID || focusedID == otherID) flagRedraw

        return $ Out VUnit s k
      _ -> badConst
    Make -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        ins <- use installedDevices
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
              MissingCatalyst -> "not installed"
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
        finishCookingRecipe recipe (WorldUpdate Right) (RobotUpdate changeInv)
      _ -> badConst
    Has -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        return $ Out (VBool ((> 0) $ countByName name inv)) s k
      _ -> badConst
    Installed -> case vs of
      [VString name] -> do
        inv <- use installedDevices
        return $ Out (VBool ((> 0) $ countByName name inv)) s k
      _ -> badConst
    Count -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        return $ Out (VInt (fromIntegral $ countByName name inv)) s k
      _ -> badConst
    Whereami -> do
      V2 x y <- use robotLocation
      return $ Out (VPair (VInt (fromIntegral x)) (VInt (fromIntegral y))) s k
    Drill -> case vs of
      [VDir d] -> do
        rname <- use robotName
        inv <- use robotInventory
        ins <- use installedDevices

        let toyDrill = lookupByName "drill" ins
            metalDrill = lookupByName "metal drill" ins
            insDrill = listToMaybe $ metalDrill <> toyDrill

        drill <- insDrill `isJustOr` Fatal "Drill is required but not installed?!"

        let directionText = case d of
              DDown -> "under"
              DForward -> "ahead of"
              DBack -> "behind"
              _ -> dirSyntax (dirInfo d) <> " of"

        (nextLoc, nextME) <- lookInDirection d
        nextE <-
          nextME
            `isJustOrFail` ["There is nothing to drill", directionText, "robot", rname <> "."]

        inRs <- use recipesIn

        let recipes = filter drilling (recipesFor inRs nextE)
            drilling = any ((== drill) . snd) . view recipeRequirements

        not (null recipes) `holdsOrFail` ["There is no way to drill", indefinite (nextE ^. entityName) <> "."]

        -- add the drilled entity so it can be consumed by the recipe
        let makeRecipe r = (,r) <$> make' (insert nextE inv, ins) r
        chosenRecipe <- weightedChoice (\((_, _), r) -> r ^. recipeWeight) (rights (map makeRecipe recipes))
        ((invTaken, outs), recipe) <-
          chosenRecipe
            `isJustOrFail` ["You don't have the ingredients to drill", indefinite (nextE ^. entityName) <> "."]

        let (out, down) = L.partition ((`hasProperty` Portable) . snd) outs
            changeInv =
              flip (L.foldl' (flip $ uncurry insertCount)) out
                . flip (L.foldl' (flip $ insertCount 0)) (map snd down)
            changeWorld = changeWorld' nextE nextLoc down

        -- take recipe inputs from inventory and add outputs after recipeTime
        robotInventory .= invTaken
        finishCookingRecipe recipe (WorldUpdate changeWorld) (RobotUpdate changeInv)
      _ -> badConst
    Blocked -> do
      loc <- use robotLocation
      orient <- use robotOrientation
      let nextLoc = loc ^+^ (orient ? zero)
      me <- entityAt nextLoc
      return $ Out (VBool (maybe False (`hasProperty` Unwalkable) me)) s k
    Scan -> case vs of
      [VDir d] -> do
        (_loc, me) <- lookInDirection d
        res <- case me of
          Nothing -> return $ VInj False VUnit
          Just e -> do
            robotInventory %= insertCount 0 e
            updateDiscoveredEntities e
            return $ VInj True (VString (e ^. entityName))

        return $ Out res s k
      _ -> badConst
    Knows -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        ins <- use installedDevices
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

        return $ Out VUnit s k
      _ -> badConst
    Random -> case vs of
      [VInt hi] -> do
        n <- uniform (0, hi - 1)
        return $ Out (VInt n) s k
      _ -> badConst
    Atomic -> case vs of
      -- To execute an atomic block, set the runningAtomic flag,
      -- push an FFinishAtomic frame so that we unset the flag when done, and
      -- proceed to execute the argument.
      [cmd] -> do
        runningAtomic .= True
        return $ Out cmd s (FExec : FFinishAtomic : k)
      _ -> badConst
    As -> case vs of
      [VRobot rid, prog] -> do
        -- Get the named robot and current game state
        r <- robotWithID rid >>= (`isJustOrFail` ["There is no robot with ID", from (show rid)])
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
      [VString rname] -> do
        r <- robotWithName rname >>= (`isJustOrFail` ["There is no robot named", rname])
        let robotValue = VRobot (r ^. robotID)
        return $ Out robotValue s k
      _ -> badConst
    RobotNumbered -> case vs of
      [VInt rid] -> do
        r <-
          robotWithID (fromIntegral rid)
            >>= (`isJustOrFail` ["There is no robot with number", from (show rid)])
        let robotValue = VRobot (r ^. robotID)
        return $ Out robotValue s k
      _ -> badConst
    Say -> case vs of
      [VString msg] -> do
        rn <- use robotName -- XXX use robot name + ID
        emitMessage (T.concat [rn, ": ", msg])
        return $ Out VUnit s k
      _ -> badConst
    Log -> case vs of
      [VString msg] -> do
        rn <- use robotName
        time <- use ticks
        robotLog %= (Seq.|> LogEntry msg rn time)
        return $ Out VUnit s k
      _ -> badConst
    View -> case vs of
      [VRobot rid] -> do
        _ <-
          robotWithID rid
            >>= (`isJustOrFail` ["There is no robot with ID", from (show rid), "to view."])

        -- Only the base can actually change the view in the UI.  Other robots can
        -- execute this command but it does nothing (at least for now).
        rn <- use robotID
        when (rn == 0) $
          viewCenterRule .= VCRobot rid

        return $ Out VUnit s k
      _ -> badConst
    Appear -> case vs of
      [VString app] -> do
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
      [VString name] -> do
        em <- use entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name <> "."]

        robotInventory %= insert e
        updateDiscoveredEntities e

        return $ Out VUnit s k
      _ -> badConst
    Ishere -> case vs of
      [VString name] -> do
        loc <- use robotLocation
        me <- entityAt loc
        case me of
          Nothing -> return $ Out (VBool False) s k
          Just e -> return $ Out (VBool (T.toLower (e ^. entityName) == T.toLower name)) s k
      _ -> badConst
    Self -> do
      rid <- use robotID
      return $ Out (VRobot rid) s k
    Parent -> do
      mp <- use robotParentID
      rid <- use robotID
      return $ Out (VRobot (fromMaybe rid mp)) s k
    Base -> return $ Out (VRobot 0) s k
    Whoami -> case vs of
      [] -> do
        name <- use robotName
        return $ Out (VString name) s k
      _ -> badConst
    Setname -> case vs of
      [VString name] -> do
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
      [VString msg] -> return $ Up (User msg) s k
      _ -> badConst
    Reprogram -> case vs of
      [VRobot childRobotID, VDelay cmd e] -> do
        r <- get
        creative <- use creativeMode

        -- check if robot exists
        childRobot <-
          robotWithID childRobotID
            >>= (`isJustOrFail` ["There is no robot with ID", from (show childRobotID) <> "."])

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
        -- creative mode ignores distance checks
        loc <- use robotLocation
        (creative || (childRobot ^. robotLocation) `manhattan` loc <= 1)
          `holdsOrFail` ["You can only reprogram an adjacent robot."]

        -- Figure out if we can supply what the target robot requires,
        -- and if so, what is needed.
        (toInstall, toGive) <-
          checkRequirements
            (r ^. robotInventory)
            (childRobot ^. robotInventory)
            (childRobot ^. installedDevices)
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
        provisionChild childRobotID (fromList . S.toList $ toInstall) toGive

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

        (toInstall, toGive) <-
          checkRequirements (r ^. robotInventory) E.empty E.empty cmd "You" FixByObtain

        -- Pick a random display name.
        displayName <- randomName
        createdAt <- getNow

        -- Construct the new robot and add it to the world.
        newRobot <-
          addURobot $
            mkRobot
              (F.Const ())
              (Just pid)
              displayName
              ["A robot built by the robot named " <> r ^. robotName <> "."]
              (r ^. robotLocation)
              ( ((r ^. robotOrientation) >>= \dir -> guard (dir /= zero) >> return dir)
                  ? east
              )
              defaultRobotDisplay
              (In cmd e s [FExec])
              []
              []
              False
              createdAt

        -- Provision the new robot with the necessary devices and inventory.
        provisionChild (newRobot ^. robotID) (fromList . S.toList $ toInstall) toGive

        -- Flag the world for a redraw and return the name of the newly constructed robot.
        flagRedraw
        return $ Out (VRobot (newRobot ^. robotID)) s k
      _ -> badConst
    Salvage -> case vs of
      [] -> do
        loc <- use robotLocation
        rm <- use robotMap
        robotSet <- use (robotsByLocation . at loc)
        let robotIDList = maybe [] IS.toList robotSet
            mtarget = find okToSalvage . mapMaybe (`IM.lookup` rm) $ robotIDList
            okToSalvage r = (r ^. robotID /= 0) && (not . isActive $ r)
        case mtarget of
          Nothing -> return $ Out VUnit s k -- Nothing to salvage
          Just target -> do
            -- Copy the salvaged robot's installed devices into its inventory, in preparation
            -- for transferring it.
            let salvageInventory = E.union (target ^. robotInventory) (target ^. installedDevices)
            robotMap . at (target ^. robotID) . traverse . robotInventory .= salvageInventory

            let salvageItems = concatMap (\(n, e) -> replicate n (e ^. entityName)) (E.elems salvageInventory)
                numItems = length salvageItems

            -- Copy over the salvaged robot's log, if we have one
            inst <- use installedDevices
            em <- use entityMap
            creative <- use creativeMode
            logger <-
              lookupEntityName "logger" em
                `isJustOr` Fatal "While executing 'salvage': there's no such thing as a logger!?"
            when (creative || inst `E.contains` logger) $ robotLog <>= target ^. robotLog

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
                giveItem item = TApp (TApp (TConst Give) (TRobot ourID)) (TString item)

            -- Reprogram and activate the salvaged robot
            robotMap . at (target ^. robotID) . traverse . machine
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
      [VString fileName] -> do
        mf <- sendIO $ mapM readFileMay [into fileName, into $ fileName <> ".sw"]

        f <- msum mf `isJustOrFail` ["File not found:", fileName]

        mt <-
          processTerm (into @Text f) `isRightOr` \err ->
            cmdExn Run ["Error in", fileName, "\n", err]

        return $ case mt of
          Nothing -> Out VUnit s k
          Just t -> initMachine' t empty s k
      _ -> badConst
    Not -> case vs of
      [VBool b] -> return $ Out (VBool (not b)) s k
      _ -> badConst
    Neg -> case vs of
      [VInt n] -> return $ Out (VInt (- n)) s k
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
      [v] -> return $ Out (VString (prettyValue v)) s k
      _ -> badConst
    Concat -> case vs of
      [VString v1, VString v2] -> return $ Out (VString (v1 <> v2)) s k
      _ -> badConst
    AppF ->
      let msg = "The operator '$' should only be a syntactic sugar and removed in elaboration:\n"
          prependMsg = (\case (Fatal e) -> Fatal $ msg <> e; exn -> exn)
       in catchError badConst (throwError . prependMsg)
 where
  badConst :: (Has (State GameState) sig m, Has (Throw Exn) sig m) => m a
  badConst =
    throwError $
      Fatal $
        T.unlines
          [ "Bad application of execConst:"
          , from (prettyCESK (Out (VCApp c (reverse vs)) s k))
          ]

  finishCookingRecipe ::
    (Has (State GameState) sig m, Has (Throw Exn) sig m) =>
    Recipe e ->
    WorldUpdate ->
    RobotUpdate ->
    m CESK
  finishCookingRecipe r wf rf = do
    time <- use ticks
    let remTime = r ^. recipeTime
    return . (if remTime <= 1 then id else Waiting (remTime + time)) $
      Out VUnit s (FImmediate wf rf : k)

  lookInDirection ::
    (Has (State GameState) sig m, Has (State Robot) sig m, Has (Error Exn) sig m) =>
    Direction ->
    m (V2 Int64, Maybe Entity)
  lookInDirection d = do
    loc <- use robotLocation
    orient <- use robotOrientation
    when (isCardinal d) $ hasCapabilityFor COrient (TDir d)
    let nextLoc = loc ^+^ applyTurn d (orient ? zero)
    (nextLoc,) <$> entityAt nextLoc

  -- Check the required devices and inventory for running the given
  -- command on a target robot.  This function is used in common by
  -- both 'Build' and 'Reprogram'.
  --
  -- It is given as inputs the parent robot inventory, the inventory
  -- and installed devices of the child (these will be empty in the
  -- case of 'Build'), and the command to be run (along with a few
  -- inputs to configure any error messages to be generated).
  --
  -- Throws an exception if it's not possible to set up the child
  -- robot with the things it needs to execute the given program.
  -- Otherwise, returns a pair consisting of the set of devices to be
  -- installed, and the inventory that should be transferred from
  -- parent to child.
  checkRequirements ::
    (Has (State GameState) sig m, Has (State Robot) sig m, Has (Error Exn) sig m) =>
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

    -- Check that all required device names exist, and fail with
    -- an exception if not
    devs <- forM devNames $ \devName ->
      E.lookupEntityName devName em `isJustOrFail` ["Unknown device required: " <> devName]

    -- Check that all required inventory entity names exist, and fail
    -- with an exception if not
    reqElems <- forM (M.assocs reqInvNames) $ \(eName, n) ->
      (n,)
        <$> ( E.lookupEntityName eName em
                `isJustOrFail` ["Unknown entity required: " <> eName]
            )
    let reqInv = E.fromElems reqElems

    let -- List of possible devices per requirement.  Devices for
        -- required capabilities come first, then singleton devices
        -- that are required directly.  This order is important since
        -- later we zip required capabilities with this list to figure
        -- out which capabilities are missing.
        capDevices = map (`deviceForCap` em) caps ++ map (: []) devs

        -- A device is OK if it is available in the inventory of the
        -- parent robot, or already installed in the child robot.
        deviceOK d = parentInventory `E.contains` d || childDevices `E.contains` d

        -- take a pair of device sets providing capabilities that is
        -- split into (AVAIL,MISSING) and if there are some available
        -- ignore missing because we only need them for error message
        ignoreOK ([], miss) = ([], miss)
        ignoreOK (ds, _miss) = (ds, [])

        (deviceSets, missingDeviceSets) =
          Lens.over both (nubOrd . map S.fromList) . unzip $
            map (ignoreOK . L.partition deviceOK) capDevices

        formatDevices = T.intercalate " or " . map (^. entityName) . S.toList
        -- capabilities not provided by any device in inventory
        missingCaps = S.fromList . map fst . filter (null . snd) $ zip caps deviceSets

        alreadyInstalled = S.fromList . map snd . E.elems $ childDevices

        -- Figure out what is missing from the required inventory
        missingChildInv = reqInv `E.difference` childInventory

    if creative
      then -- In creative mode, just return ALL the devices
        return (S.unions (map S.fromList capDevices) `S.difference` alreadyInstalled, missingChildInv)
      else do
        -- check if robot has all devices to execute new command
        all null missingDeviceSets
          `holdsOrFail` ( singularSubjectVerb subject "do" :
                          "not have required devices, please" :
                          formatIncapableFix fixI <> ":" :
                          (("\n  - " <>) . formatDevices <$> filter (not . null) missingDeviceSets)
                        )
        -- check that there are in fact devices to provide every required capability
        not (any null deviceSets) `holdsOr` Incapable fixI (R.Requirements missingCaps S.empty M.empty) cmd

        let minimalInstallSet = smallHittingSet (filter (S.null . S.intersection alreadyInstalled) deviceSets)

            -- Check that we have enough in our inventory to cover the
            -- required installs PLUS what's missing from the child
            -- inventory.

            -- What do we need?
            neededParentInv =
              missingChildInv
                `E.union` (fromList . S.toList $ minimalInstallSet)

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

        return (minimalInstallSet, missingChildInv)

  -- replace some entity in the world with another entity
  changeWorld' ::
    Entity ->
    V2 Int64 ->
    IngredientList Entity ->
    W.World Int Entity ->
    Either Exn (W.World Int Entity)
  changeWorld' eThen loc down w =
    let eNow = W.lookupEntity (W.locToCoords loc) w
     in if Just eThen /= eNow
          then Left $ cmdExn c ["The", eThen ^. entityName, "is not there."]
          else
            w `updateLoc` loc <$> case down of
              [] -> Right Nothing
              [de] -> Right $ Just $ snd de
              _ -> Left $ Fatal "Bad recipe:\n more than one unmovable entity produced."

  destroyIfNotBase :: (Has (State Robot) sig m, Has (Error Exn) sig m) => m ()
  destroyIfNotBase = do
    destroy <- isDestroyable =<< get @Robot
    when destroy $ selfDestruct .= True

  isDestroyable :: (Has (Error Exn) sig m) => Robot -> m Bool
  isDestroyable r = do
    if r ^. robotID == 0
      then throwError $ cmdExn c ["You consider destroying your base, but decide not to do it after all."]
      else return True

  getRobotWithinTouch ::
    (Has (State Robot) sig m, Has (State GameState) sig m, Has (Error Exn) sig m) =>
    RID ->
    m Robot
  getRobotWithinTouch rid = do
    mother <- robotWithID rid
    other <- mother `isJustOrFail` ["There is no robot with ID", from (show rid) <> "."]
    -- Make sure it is in the same location
    loc <- use robotLocation
    ((other ^. robotLocation) `manhattan` loc <= 1)
      `holdsOrFail` ["The robot with ID", from (show rid), "is not close enough."]
    return other

  -- update some tile in the world setting it to entity or making it empty
  updateLoc w loc res = W.update (W.locToCoords loc) (const res) w

  holdsOrFail :: (Has (Throw Exn) sig m) => Bool -> [Text] -> m ()
  holdsOrFail a ts = a `holdsOr` cmdExn c ts

  isJustOrFail :: (Has (Throw Exn) sig m) => Maybe a -> [Text] -> m a
  isJustOrFail a ts = a `isJustOr` cmdExn c ts

  returnEvalCmp = case vs of
    [v1, v2] -> (\b -> Out (VBool b) s k) <$> evalCmp c v1 v2
    _ -> badConst
  returnEvalArith = case vs of
    [VInt n1, VInt n2] -> (\r -> Out (VInt r) s k) <$> evalArith c n1 n2
    _ -> badConst

  -- The code for grab and harvest is almost identical, hence factored
  -- out here.
  doGrab shouldHarvest = do
    -- Ensure there is an entity here.
    loc <- use robotLocation
    e <- entityAt loc >>= (`isJustOrFail` ["There is nothing here to grab."])

    -- Ensure it can be picked up.
    (e `hasProperty` Portable)
      `holdsOrFail` ["The", e ^. entityName, "here can't be grabbed."]

    -- Remove the entity from the world.
    updateEntityAt loc (const Nothing)
    flagRedraw

    -- Immediately regenerate entities with 'infinite' property.
    when (e `hasProperty` Infinite) $
      updateEntityAt loc (const (Just e))

    -- Possibly regrow the entity, if it is growable and the 'harvest'
    -- command was used.
    when ((e `hasProperty` Growable) && shouldHarvest) $ do
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
    return $ Out (VString (e' ^. entityName)) s k

-- | Give some entities from a parent robot (the robot represented by
--   the ambient @State Robot@ effect) to a child robot (represented
--   by the given 'RID') as part of a 'Build' or 'Reprogram' command.
--   The first 'Inventory' is devices to be installed, and the second
--   is entities to be transferred.
--
--   In classic mode, the entities will be /transferred/ (that is,
--   removed from the parent robot's inventory); in creative mode, the
--   entities will be copied/created, that is, no entities will be
--   removed from the parent robot.
provisionChild ::
  (Has (State Robot) sig m, Has (State GameState) sig m) =>
  RID ->
  Inventory ->
  Inventory ->
  m ()
provisionChild childID toInstall toGive = do
  -- Install and give devices to child
  robotMap . ix childID . installedDevices %= E.union toInstall
  robotMap . ix childID . robotInventory %= E.union toGive

  -- Delete all items from parent in classic mode
  creative <- use creativeMode
  unless creative $
    robotInventory %= (`E.difference` (toInstall `E.union` toGive))

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
    _ -> const $ throwError $ Fatal $ T.append "evalCmp called on bad constant " (from (show c))

-- | Compare two values, returning an 'Ordering' if they can be
--   compared, or @Nothing@ if they cannot.
compareValues :: Has (Throw Exn) sig m => Value -> Value -> m Ordering
compareValues v1 = case v1 of
  VUnit -> \case VUnit -> return EQ; v2 -> incompatCmp VUnit v2
  VInt n1 -> \case VInt n2 -> return (compare n1 n2); v2 -> incompatCmp v1 v2
  VString t1 -> \case VString t2 -> return (compare t1 t2); v2 -> incompatCmp v1 v2
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
  VClo {} -> incomparable v1
  VCApp {} -> incomparable v1
  VDef {} -> incomparable v1
  VResult {} -> incomparable v1
  VBind {} -> incomparable v1
  VDelay {} -> incomparable v1
  VRef {} -> incomparable v1

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
    CmdFailed Lt $
      T.unwords ["Comparison is undefined for ", prettyValue v1, "and", prettyValue v2]

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
safeDiv _ 0 = throwError $ CmdFailed Div "Division by zero"
safeDiv a b = return $ a `div` b

-- | Perform exponentiation, but return @Nothing@ if the power is negative.
safeExp :: Has (Throw Exn) sig m => Integer -> Integer -> m Integer
safeExp a b
  | b < 0 = throwError $ CmdFailed Exp "Negative exponent"
  | otherwise = return $ a ^ b

-- | Update the global list of discovered entities, and check for new recipes.
updateDiscoveredEntities :: (Has (State GameState) sig m, Has (State Robot) sig m) => Entity -> m ()
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
  let newCaps = S.fromList (e ^. entityCapabilities)
      keepConsts = \case
        Just cap -> cap `S.member` newCaps
        Nothing -> False
      entityConsts = filter (keepConsts . constCaps) allConst
  knownCommands <- use (availableCommands . notificationsContent)
  let newCommands = filter (`notElem` knownCommands) entityConsts
      newCount = length newCommands
  availableCommands %= mappend (Notifications newCount newCommands)
