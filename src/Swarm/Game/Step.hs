{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

import Control.Lens hiding (Const, from, parts, use, uses, view, (%=), (+=), (.=), (<>=))
import Data.Bool (bool)
import Data.Either (rights)
import Data.Int (Int64)
import Data.List (find)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Linear (V2 (..), zero, (^+^))
import System.Random (UniformRange, uniformR)
import Witch
import Prelude hiding (lookup)

import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import qualified Swarm.Game.Entity as E
import Swarm.Game.Exception
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Value
import qualified Swarm.Game.World as W
import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Pipeline
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax
import Swarm.Util

import Control.Carrier.Error.Either (runError)
import Control.Carrier.State.Lazy
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Monad (forM_, msum, unless, void, when)

-- | The maximum number of CESK machine evaluation steps each robot is
--   allowed during a single game tick.
evalStepsPerTick :: Int
evalStepsPerTick = 100

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
  forM_ robotNames $ \rn -> do
    mr <- uses robotMap (M.lookup rn)
    case mr of
      Nothing -> return ()
      Just curRobot -> do
        curRobot' <- tickRobot curRobot
        if curRobot' ^. selfDestruct
          then deleteRobot rn
          else do
            robotMap %= M.insert rn curRobot'
            let oldLoc = curRobot ^. robotLocation
                newLoc = curRobot' ^. robotLocation

                -- Make sure empty sets don't hang around in the
                -- robotsByLocation map.  We don't want a key with an
                -- empty set at every location any robot has ever
                -- visited!
                deleteOne _ Nothing = Nothing
                deleteOne x (Just s)
                  | S.null s' = Nothing
                  | otherwise = Just s'
                 where
                  s' = S.delete x s

            when (newLoc /= oldLoc) $ do
              robotsByLocation . at oldLoc %= deleteOne rn
              robotsByLocation . at newLoc . non Empty %= S.insert rn
            case waitingUntil curRobot' of
              Just wakeUpTime ->
                sleepUntil rn wakeUpTime
              Nothing ->
                unless (isActive curRobot') do
                  sleepForever rn

  -- See if the base is finished with a computation, and if so, record
  -- the result in the game state so it can be displayed by the REPL.
  mr <- use (robotMap . at "base")
  case mr of
    Just r -> do
      res <- use replStatus
      case res of
        REPLWorking ty Nothing -> replStatus .= REPLWorking ty (getResult r)
        _otherREPLStatus -> return ()
    Nothing -> return ()

  -- Possibly update the view center.
  modify recalcViewCenter
  -- Advance the game time by one.
  ticks += 1

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

-- | Get the robot with a given name (if any).
robotNamed :: (Has (State GameState) sig m) => Text -> m (Maybe Robot)
robotNamed nm = use (robotMap . at nm)

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
ensureCanExecute c = do
  mode <- use gameMode
  sys <- use systemRobot
  robotCaps <- use robotCapabilities
  let missingCaps = constCaps c `S.difference` robotCaps
  (sys || mode == Creative || S.null missingCaps)
    `holdsOr` Incapable missingCaps (TConst c)

-- | Test whether the current robot has a given capability (either
--   because it has a device which gives it that capability, or it is a
--   system robot, or we are in creative mode).
hasCapability :: (Has (State Robot) sig m, Has (State GameState) sig m) => Capability -> m Bool
hasCapability cap = do
  mode <- use gameMode
  sys <- use systemRobot
  caps <- use robotCapabilities
  return (sys || mode == Creative || cap `S.member` caps)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityOr ::
  (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Capability -> Exn -> m ()
hasCapabilityOr cap exn = do
  h <- hasCapability cap
  h `holdsOr` exn

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
--   'evalStepsPerTick' CESK machine steps and at most one command
--   execution.
tickRobot :: (Has (State GameState) sig m, Has (Lift IO) sig m) => Robot -> m Robot
tickRobot = tickRobotRec . (tickSteps .~ evalStepsPerTick)

-- | Recursive helper function for 'tickRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
tickRobotRec :: (Has (State GameState) sig m, Has (Lift IO) sig m) => Robot -> m Robot
tickRobotRec r
  | not (isActive r) || r ^. tickSteps <= 0 = return r
  | otherwise = stepRobot r >>= tickRobotRec

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
  -- (liftIO $ appendFile "out.txt" (prettyCESK cesk)) >>

  -- It's a little unsatisfactory the way we handle having both Robot
  -- and GameState in different states (by having one be concrete and
  -- the other accessible via 'lift').  Having them both be capability
  -- constraints is what we really want, but it's not possible to do
  -- that with mtl.  Ultimately we may want to switch to an effects
  -- library.  I am hesitant to use polysemy because of performance
  -- issues.  Perhaps fused-effects would work.  I really want to use
  -- 'eff' but seems like it's not ready yet.

  ------------------------------------------------------------
  -- Evaluation

  -- We wake up robots whose wake-up time has been reached. If it hasn't yet
  -- then stepCESK is a no-op.
  Waiting wakeupTime cesk' -> do
    time <- use ticks
    if wakeupTime == time
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
  -- A constant is turned into a VCApp which might be waiting for arguments.
  In (TConst c) _ s k -> return $ Out (VCApp c []) s k
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
    CEnv `hasCapabilityOr` Incapable (S.singleton CEnv) tm
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
  -- To execute a constant application, delegate to the 'execConst'
  -- function.  Set tickSteps to 0 if the command is supposed to take
  -- a tick, so the robot won't take any more steps this tick.
  Out (VCApp c args) s (FExec : k) -> do
    when (takesTick c) $ tickSteps .= 0
    res <- runError (execConst c (reverse args) s k)
    case res of
      Left exn -> return $ Up exn s k
      Right cek' -> return cek'

  -- To execute a bind expression, evaluate and execute the first
  -- command, and remember the second for execution later.
  Out (VBind mx c1 c2 e) s (FExec : k) -> return $ In c1 e s (FExec : FBind mx c2 e : k)
  -- If first command completes with a value along with an environment
  -- resulting from definition commands, switch to evaluating the
  -- second command of the bind.  Extend the environment with both the
  -- definition environment resulting from the first command, as well
  -- as a binding for the result (if the bind was of the form @x <-
  -- c1; c2@).  Remember that we must execute the second command once
  -- it has been evaluated, then union any resulting definition
  -- environment with the definition environment from the first
  -- command.
  Out (VResult v ve) s (FBind mx t2 e : k) ->
    return $ In t2 (maybe id (`addBinding` v) mx . (`union` ve) $ e) s (FExec : FUnionEnv ve : k)
  -- On the other hand, if the first command completes with a simple value,
  -- we do something similar, but don't have to worry about the environment.
  Out v s (FBind mx t2 e : k) ->
    return $ In t2 (maybe id (`addBinding` v) mx e) s (FExec : k)
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
  -- environment and type and capability contexts into the robot's
  -- top-level environment and contexts, so they will be available to
  -- future programs.
  Out (VResult v e) s (FLoadEnv ctx cctx : k) -> do
    robotContext . defVals %= (`union` e)
    robotContext . defTypes %= (`union` ctx)
    robotContext . defCaps %= (`union` cctx)
    return $ Out v s k
  Out v s (FLoadEnv {} : k) -> return $ Out v s k
  -- Any other type of value wiwth an FExec frame is an error (should
  -- never happen).
  Out _ s (FExec : _) -> badMachineState s "FExec frame with non-executable value"
  -- Any other frame with a VResult is an error (should never happen).
  Out (VResult _ _) s _ -> badMachineState s "no appropriate stack frame to catch a VResult"
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
  Up exn s [] -> do
    h <- hasCapability CLog
    case h of
      True -> return $ In (TApp (TConst Log) (TString (formatExn exn))) empty s [FExec]
      False -> return $ Out VUnit s []
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

-- | Determine whether a constant should take up a tick or not when executed.
takesTick :: Const -> Bool
takesTick c = isCmd c && (c `notElem` [Selfdestruct, Noop, Return, Whereami, Blocked, Ishere, Try, Random, Appear])

-- | At the level of the CESK machine, the only difference bewteen
--   between *evaluating* a function constant and *executing* a
--   command constant is what kind of exceptions can be thrown.  When
--   evaluating, the only thing that could throw an exception is
--   trying to use a function constant without the proper capability
--   (for example, trying to use `if` without having a conditional
--   device).  Any other exceptions constitute a bug.
evalConst ::
  (Has (State GameState) sig m, Has (State Robot) sig m, Has (Lift IO) sig m) => Const -> [Value] -> Store -> Cont -> m CESK
evalConst c vs s k = do
  res <- runError $ execConst c vs s k
  case res of
    Left exn@Fatal {} -> return $ Up exn s k
    Left exn@Incapable {} -> return $ Up exn s k
    Left exn -> do
      let msg =
            T.unlines
              [ "evalConst shouldn't be able to throw this kind of exception:"
              , formatExn exn
              ]
      return $ Up (Fatal msg) s k
    Right cek' -> return cek'

-- | A system program for a "seed robot", to regrow a growable entity
--   after it is harvested.
seedProgram :: Integer -> Integer -> Text -> ProcessedTerm
seedProgram minTime randTime thing =
  [tmQ|
  {
    r <- random (1 + $int:randTime);
    wait (r + $int:minTime);
    appear "|";
    r <- random (1 + $int:randTime);
    wait (r + $int:minTime);
    place $str:thing;
    selfdestruct
  }
  |]

-- | Construct a "seed robot" from entity, time range and position.
--   It has low priority and will be covered by placed entities.
mkSeedBot :: Entity -> (Integer, Integer) -> V2 Int64 -> Robot
mkSeedBot e (minT, maxT) loc =
  mkRobot
    "seed"
    loc
    (V2 0 0)
    (initMachine (seedProgram minT (maxT - minT) (e ^. entityName)) empty)
    []
    & robotDisplay
      .~ ( defaultEntityDisplay '.'
            & displayAttr .~ (e ^. entityDisplay . displayAttr)
            & displayPriority .~ 0
         )
    & robotInventory .~ E.singleton e
    & systemRobot .~ True

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
      selfDestruct .= True
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
          when (e `hasProperty` Liquid && CFloat `S.notMember` caps) $
            selfDestruct .= True

      robotLocation .= nextLoc
      flagRedraw
      return $ Out VUnit s k
    Grab -> do
      -- Ensure there is an entity here.
      loc <- use robotLocation
      e <- entityAt loc >>= (`isJustOrFail` ["There is nothing here to grab."])

      -- Ensure it can be picked up.
      (e `hasProperty` Portable)
        `holdsOrFail` ["The", e ^. entityName, "here can't be grabbed."]

      -- Remove the entity from the world.
      updateEntityAt loc (const Nothing)
      flagRedraw

      -- Possibly regrow the entity.
      when (e `hasProperty` Growable) $ do
        let GrowthTime (minT, maxT) = (e ^. entityGrowth) ? defaultGrowthTime

        if maxT == 0
          then -- Special case: if the time is zero, growth is instant.
            updateEntityAt loc (const (Just e))
          else -- Otherwise, grow a new entity from a seed.
            void $ addRobot $ mkSeedBot e (minT, maxT) loc

      -- Add the picked up item to the robot's inventory.  If the
      -- entity yields something different, add that instead.
      let yieldName = e ^. entityYields
      e' <- case yieldName of
        Nothing -> return e
        Just n -> (?e) <$> uses entityMap (lookupEntityName n)

      robotInventory %= insert e'

      -- Return the name of the item grabbed.
      return $ Out (VString (e ^. entityName)) s k
    Turn -> case vs of
      [VDir d] -> do
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
            `isJustOrFail` ["What is", indefinite name, "?"]

        (E.lookup e inv > 0)
          `holdsOrFail` ["You don't have", indefinite name, "to place."]

        -- Place the entity and remove it from the inventory
        updateEntityAt loc (const (Just e))
        robotInventory %= delete e

        flagRedraw
        return $ Out VUnit s k
      _ -> badConst
    Give -> case vs of
      [VString otherName, VString itemName] -> do
        -- Make sure the other robot exists
        other <-
          robotNamed otherName
            >>= (`isJustOrFail` ["There is no robot named", otherName, "."])

        -- Make sure it is in the same location
        loc <- use robotLocation
        ((other ^. robotLocation) `manhattan` loc <= 1)
          `holdsOrFail` ["The robot named", otherName, "is not close enough."]

        -- Make sure we have the required item
        inv <- use robotInventory
        item <-
          (listToMaybe . lookupByName itemName $ inv)
            `isJustOrFail` ["What is", indefinite itemName, "?"]

        (E.lookup item inv > 0)
          `holdsOrFail` ["You don't have", indefinite itemName, "to give."]

        -- Giving something to ourself should be a no-op.  We need
        -- this as a special case since it will not work to modify
        -- ourselves in the robotMap --- after performing a tick we
        -- return a modified Robot which gets put back in the
        -- robotMap, overwriting any changes to this robot made
        -- directly in the robotMap during the tick.
        myName <- use robotName
        focusedName <- use focusedRobotName
        when (otherName /= myName) $ do
          -- Make the exchange
          robotMap . at otherName . _Just . robotInventory %= insert item
          robotInventory %= delete item

          -- Flag the UI for a redraw if we are currently showing either robot's inventory
          when (focusedName == myName || focusedName == otherName) flagRedraw

        return $ Out VUnit s k
      _ -> badConst
    Install -> case vs of
      [VString otherName, VString itemName] -> do
        -- Make sure the other robot exists
        other <-
          robotNamed otherName
            >>= (`isJustOrFail` ["There is no robot named", otherName, "."])

        -- Make sure it is in the same location
        loc <- use robotLocation
        ((other ^. robotLocation) `manhattan` loc <= 1)
          `holdsOrFail` ["The robot named", otherName, "is not close enough."]

        -- Make sure we have the required item
        inv <- use robotInventory
        item <-
          (listToMaybe . lookupByName itemName $ inv)
            `isJustOrFail` ["What is", indefinite itemName, "?"]

        (E.lookup item inv > 0)
          `holdsOrFail` ["You don't have", indefinite itemName, "to install."]

        myName <- use robotName
        focusedName <- use focusedRobotName
        case otherName == myName of
          -- We have to special case installing something on ourselves
          -- for the same reason as Give.
          True -> do
            -- Don't do anything if the robot already has the device.
            already <- use (installedDevices . to (`E.contains` item))
            unless already $ do
              installedDevices %= insert item
              robotInventory %= delete item

              -- Flag the UI for a redraw if we are currently showing our inventory
              when (focusedName == myName) flagRedraw
          False -> do
            let otherDevices = robotMap . at otherName . _Just . installedDevices
            already <- use $ pre (otherDevices . to (`E.contains` item))
            unless (already == Just True) $ do
              robotMap . at otherName . _Just . installedDevices %= insert item
              robotInventory %= delete item

              -- Flag the UI for a redraw if we are currently showing
              -- either robot's inventory
              when (focusedName == myName || focusedName == otherName) flagRedraw

        return $ Out VUnit s k
      _ -> badConst
    Make -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        ins <- use installedDevices
        em <- use entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name, "."]

        outRs <- use recipesOut

        -- Only consider recipes where the number of things we are trying to make
        -- is greater in the outputs than in the inputs.  This prevents us from doing
        -- silly things like making copper pipes when the user says "make furnace".
        let recipes = filter increase (recipesFor outRs e)
            increase r = countIn (r ^. recipeOutputs) > countIn (r ^. recipeInputs)
            countIn xs = maybe 0 fst (find ((== e) . snd) xs)
        not (null recipes)
          `holdsOrFail` ["There is no known recipe for making", indefinite name, "."]

        -- Try recipes and remeber the first one that we have ingredients for.
        (invTaken, changeInv, recipe) <-
          listToMaybe (rights (map (make (inv, ins)) recipes))
            `isJustOrFail` ["You don't have the ingredients to make", indefinite name, "."]

        -- take recipe inputs from inventory and add outputs after recipeTime
        robotInventory .= invTaken
        finishCookingRecipe recipe (WorldUpdate Right) (RobotUpdate changeInv)
      _ -> badConst
    Whereami -> do
      V2 x y <- use robotLocation
      return $ Out (VPair (VInt (fromIntegral x)) (VInt (fromIntegral y))) s k
    Drill -> case vs of
      [VDir d] -> do
        rname <- use robotName
        inv <- use robotInventory
        ins <- use installedDevices
        loc <- use robotLocation
        rDir <- use robotOrientation

        let nextLoc = loc ^+^ applyTurn d (rDir ? V2 0 0)
        em <- use entityMap
        drill <- lookupEntityName "drill" em `isJustOr` Fatal "Drill does not exist?!"
        nextME <- entityAt nextLoc
        nextE <-
          nextME
            `isJustOrFail` ["There is nothing to drill", "in the direction", "of robot", rname <> "."]

        inRs <- use recipesIn

        let recipes = filter drilling (recipesFor inRs nextE)
            drilling = any ((== drill) . snd) . view recipeRequirements

        not (null recipes) `holdsOrFail` ["There is no way to drill", indefinite (nextE ^. entityName) <> "."]

        -- add the drilled entity so it can be consumed by the recipe
        let makeRecipe r = (,r) <$> make' (insert nextE inv, ins) r
        ((invTaken, outs), recipe) <-
          listToMaybe (rights (map makeRecipe recipes))
            `isJustOrFail` ["You don't have the ingredients to drill", indefinite (nextE ^. entityName) <> "."]

        let (out, down) = L.partition ((`hasProperty` Portable) . snd) outs
            changeInv inv' = L.foldl' (flip $ uncurry insertCount) inv' out
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
        loc <- use robotLocation
        orient <- use robotOrientation
        let scanLoc = loc ^+^ applyTurn d (orient ? zero)
        me <- entityAt scanLoc
        case me of
          Nothing -> return ()
          Just e -> robotInventory %= insertCount 0 e

        return $ Out VUnit s k
      _ -> badConst
    Upload -> case vs of
      [VString otherName] -> do
        -- Make sure the other robot exists
        other <-
          robotNamed otherName
            >>= (`isJustOrFail` ["There is no robot named", otherName, "."])

        -- Make sure it is in the same location
        loc <- use robotLocation
        ((other ^. robotLocation) `manhattan` loc <= 1)
          `holdsOrFail` ["The robot named", otherName, "is not close enough."]

        -- Upload knowledge of everything in our inventory
        inv <- use robotInventory
        forM_ (elems inv) $ \(_, e) ->
          robotMap . at otherName . _Just . robotInventory %= insertCount 0 e

        -- Upload our log
        rlog <- use robotLog
        robotMap . at otherName . _Just . robotLog <>= rlog

        return $ Out VUnit s k
      _ -> badConst
    Random -> case vs of
      [VInt hi] -> do
        n <- uniform (0, hi - 1)
        return $ Out (VInt n) s k
      _ -> badConst
    Say -> case vs of
      [VString msg] -> do
        rn <- use robotName
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
      [VString name] -> do
        _ <-
          robotNamed name
            >>= (`isJustOrFail` ["There is no robot named ", name, " to view."])

        -- Only the base can actually change the view in the UI.  Other robots can
        -- execute this command but it does nothing (at least for now).
        rn <- use robotName
        when (rn == "base") $
          viewCenterRule .= VCRobot name

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
          _other -> raise Appear [quote app, "is not a valid appearance string."]
      _ -> badConst
    Create -> case vs of
      [VString name] -> do
        em <- use entityMap
        e <-
          lookupEntityName name em
            `isJustOrFail` ["I've never heard of", indefiniteQ name, "."]

        robotInventory %= insert e
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
    Whoami -> case vs of
      [] -> do
        name <- use robotName
        return $ Out (VString name) s k
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
          Just (E t e') -> return $ In t e' (setCell loc Blackhole s) (FUpdate loc : k)
          -- If the location contains a Blackhole, that means we are
          -- already currently in the middle of evaluating it, i.e. it
          -- depends on itself, so throw an 'InfiniteLoop' error.
          Just Blackhole -> return $ Up InfiniteLoop s k
          -- If the location already contains a value, just return it.
          Just (V v) -> return $ Out v s k
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
    Raise -> case vs of
      [VString msg] -> return $ Up (User msg) s k
      _ -> badConst
    Reprogram -> case vs of
      [VString childRobotName, VDelay cmd e] -> do
        r <- get
        em <- use entityMap
        mode <- use gameMode

        -- check if robot exists
        childRobot <-
          robotNamed childRobotName
            >>= (`isJustOrFail` ["There is no robot named", childRobotName, "."])

        -- check that current robot is not trying to reprogram self
        myName <- use robotName
        (childRobotName /= myName)
          `holdsOrFail` ["You cannot make a robot reprogram itself"]

        -- check if robot has completed executing it's current command
        _ <-
          finalValue (childRobot ^. machine)
            `isJustOrFail` ["You cannot reprogram a robot that has not completed its current command"]

        -- check if childRobot is at the correct distance
        -- a robot can program adjacent robots
        -- creative mode ignores distance checks
        loc <- use robotLocation
        ( mode == Creative
            || (childRobot ^. robotLocation) `manhattan` loc <= 1
          )
          `holdsOrFail` ["You can only program adjacent robot"]

        let -- Find out what capabilities are required by the program that will
            -- be run on the other robot, and what devices would provide those
            -- capabilities.
            (caps, _capCtx) = requiredCaps (r ^. robotContext . defCaps) cmd
            capDevices = S.fromList . mapMaybe (`deviceForCap` em) . S.toList $ caps

            -- device is ok if it is installed on the childRobot
            deviceOK d = (childRobot ^. installedDevices) `E.contains` d

            missingDevices = S.filter (not . deviceOK) capDevices

        -- check if robot has all devices to execute new command
        (mode == Creative || S.null missingDevices)
          `holdsOrFail` [ "the target robot does not have required devices:\n"
                        , commaList (map (^. entityName) (S.toList missingDevices))
                        ]

        -- update other robot's CESK machine, environment and context
        -- the childRobot inherits the parent robot's environment
        -- and context which collectively mean all the variables
        -- declared in the parent robot
        robotMap . at childRobotName . _Just . machine .= In cmd e s [FExec]
        robotMap . at childRobotName . _Just . robotContext .= r ^. robotContext

        return $ Out VUnit s k
      _ -> badConst
    Build -> case vs of
      [VString name, VDelay cmd e] -> do
        r <- get
        em <- use entityMap
        mode <- use gameMode

        let -- Standard devices that are always installed.
            -- XXX in the future, make a way to build these and just start the base
            -- out with a large supply of each?
            stdDeviceList =
              ["treads", "grabber", "solar panel", "detonator", "scanner", "plasma cutter"]
            stdDevices = S.fromList $ mapMaybe (`lookupEntityName` em) stdDeviceList

            -- Find out what capabilities are required by the program that will
            -- be run on the newly constructed robot, and what devices would
            -- provide those capabilities.
            (caps, _capCtx) = requiredCaps (r ^. robotContext . defCaps) cmd
            capDevices = S.fromList . mapMaybe (`deviceForCap` em) . S.toList $ caps

            -- Note that _capCtx must be empty: at least at the
            -- moment, definitions are only allowed at the top level,
            -- so there can't be any inside the argument to build.
            -- (Though perhaps there is an argument that this ought to
            -- be relaxed specifically in the case of 'Build'.)

            -- The devices that need to be installed on the new robot is the union
            -- of these two sets.
            devices = stdDevices `S.union` capDevices

            -- A device is OK to install if it is a standard device, or we have one
            -- in our inventory.
            deviceOK d = d `S.member` stdDevices || (r ^. robotInventory) `E.contains` d

            missingDevices = S.filter (not . deviceOK) capDevices

        -- Make sure we're not missing any required devices.
        (mode == Creative || S.null missingDevices)
          `holdsOrFail` [ "this would require installing devices you don't have:\n"
                        , commaList (map (^. entityName) (S.toList missingDevices))
                        ]

        -- Construct the new robot.
        let newRobot =
              mkRobot
                name
                (r ^. robotLocation)
                (r ^. robotOrientation ? east)
                (In cmd e s [FExec])
                (S.toList devices)

        -- Add the new robot to the world.
        newRobot' <- addRobot newRobot

        -- Remove from the inventory any devices which were installed on the new robot,
        -- if not in creative mode.
        unless (mode == Creative) $
          forM_ (devices `S.difference` stdDevices) $ \d ->
            robotInventory %= delete d

        -- Flag the world for a redraw and return the name of the newly constructed robot.
        flagRedraw
        return $ Out (VString (newRobot' ^. robotName)) s k
      _ -> badConst
    Salvage -> case vs of
      [] -> do
        loc <- use robotLocation
        rm <- use robotMap
        robotSet <- use (robotsByLocation . at loc)
        let robotNameList = maybe [] S.toList robotSet
            mtarget = find okToSalvage . mapMaybe (`M.lookup` rm) $ robotNameList
            okToSalvage r = (r ^. robotName /= "base") && (not . isActive $ r)
        case mtarget of
          Nothing -> return $ Out VUnit s k -- Nothing to salvage
          Just target -> do
            -- Copy over the salvaged robot's inventory
            robotInventory %= E.union (target ^. robotInventory)
            robotInventory %= E.union (target ^. installedDevices)

            -- Also copy over its log, if we have one
            inst <- use installedDevices
            em <- use entityMap
            mode <- use gameMode
            logger <-
              lookupEntityName "logger" em
                `isJustOr` Fatal "While executing 'salvage': there's no such thing as a logger!?"
            when (mode == Creative || inst `E.contains` logger) $ robotLog <>= target ^. robotLog

            -- Finally, delete the salvaged robot
            deleteRobot (target ^. robotName)

            return $ Out VUnit s k
      _ -> badConst
    -- run can take both types of text inputs
    -- with and without file extension as in
    -- "./path/to/file.sw" and "./path/to/file"
    Run -> case vs of
      [VString fileName] -> do
        mf <- sendIO $ mapM readFileMay [into fileName, into $ fileName <> ".sw"]

        f <- msum mf `isJustOrFail` ["File not found:", fileName]

        t <-
          processTerm (into @Text f) `isRightOr` \err ->
            cmdExn Run ["Error in", fileName, "\n", err]

        return $ initMachine' t empty k
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
    Add -> returnEvalArith
    Sub -> returnEvalArith
    Mul -> returnEvalArith
    Div -> returnEvalArith
    Exp -> returnEvalArith
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

  -- update some tile in the world setting it to entity or making it empty
  updateLoc w loc res = W.update (W.locToCoords loc) (const res) w

  holdsOrFail a ts = a `holdsOr` cmdExn c ts
  isJustOrFail a ts = a `isJustOr` cmdExn c ts

  returnEvalCmp = case vs of
    [v1, v2] ->
      case evalCmp c v1 v2 of
        Left exn -> return $ Up exn s k
        Right b -> return $ Out (VBool b) s k
    _ -> badConst
  returnEvalArith = case vs of
    [VInt n1, VInt n2] -> case evalArith c n1 n2 of
      Left exn -> return $ Up exn s k
      Right r -> return $ Out (VInt r) s k
    _ -> badConst

-- | Evaluate the application of a comparison operator.  Returns
--   @Nothing@ if the application does not make sense.
evalCmp :: Const -> Value -> Value -> Either Exn Bool
evalCmp c v1 v2 = decideCmp c $ compareValues v1 v2
 where
  decideCmp = \case
    Eq -> fmap (== EQ)
    Neq -> fmap (/= EQ)
    Lt -> fmap (== LT)
    Gt -> fmap (== GT)
    Leq -> fmap (/= GT)
    Geq -> fmap (/= LT)
    _ -> const $ Left $ Fatal $ T.append "evalCmp called on bad constant " (from (show c))

-- | Compare two values, returning an 'Ordering' if they can be
--   compared, or @Nothing@ if they cannot.
compareValues :: Value -> Value -> Either Exn Ordering
compareValues = \v1 -> case v1 of
  VUnit -> \case VUnit -> Right EQ; v2 -> Left $ incompatCmp VUnit v2
  VInt n1 -> \case VInt n2 -> Right (compare n1 n2); v2 -> Left $ incompatCmp v1 v2
  VString t1 -> \case VString t2 -> Right (compare t1 t2); v2 -> Left $ incompatCmp v1 v2
  VDir d1 -> \case VDir d2 -> Right (compare d1 d2); v2 -> Left $ incompatCmp v1 v2
  VBool b1 -> \case VBool b2 -> Right (compare b1 b2); v2 -> Left $ incompatCmp v1 v2
  VInj s1 v1' -> \case
    VInj s2 v2' ->
      case compare s1 s2 of
        EQ -> compareValues v1' v2'
        o -> Right o
    v2 -> Left $ incompatCmp v1 v2
  VPair v11 v12 -> \case
    VPair v21 v22 ->
      (<>) <$> compareValues v11 v21 <*> compareValues v12 v22
    v2 -> Left $ incompatCmp v1 v2
  VClo {} -> Left . incomparable v1
  VCApp {} -> Left . incomparable v1
  VDef {} -> Left . incomparable v1
  VResult {} -> Left . incomparable v1
  VBind {} -> Left . incomparable v1
  VDelay {} -> Left . incomparable v1
  VRef {} -> Left . incomparable v1

-- | Values with different types were compared; this should not be
--   possible since the type system should catch it.
incompatCmp :: Value -> Value -> Exn
incompatCmp v1 v2 =
  Fatal $
    T.unwords ["Incompatible comparison of ", prettyValue v1, "and", prettyValue v2]

-- | Values were compared of a type which cannot be compared
--   (e.g. functions, etc.).
incomparable :: Value -> Value -> Exn
incomparable v1 v2 =
  CmdFailed Lt $
    T.unwords ["Comparison is undefined for ", prettyValue v1, "and", prettyValue v2]

-- | Evaluate the application of an arithmetic operator, returning
--   an exception in the case of a failing operation, or in case we
--   incorrectly use it on a bad 'Const' in the library.
evalArith :: Const -> Integer -> Integer -> Either Exn Integer
evalArith = \case
  Add -> ok (+)
  Sub -> ok (-)
  Mul -> ok (*)
  Div -> safeDiv
  Exp -> safeExp
  c -> \_ _ -> Left $ Fatal $ T.append "evalArith called on bad constant " (from (show c))
 where
  ok f x y = Right $ f x y

-- | Perform an integer division, but return @Nothing@ for division by
--   zero.
safeDiv :: Integer -> Integer -> Either Exn Integer
safeDiv _ 0 = Left $ CmdFailed Div "Division by zero"
safeDiv a b = return $ a `div` b

-- | Perform exponentiation, but return @Nothing@ if the power is negative.
safeExp :: Integer -> Integer -> Either Exn Integer
safeExp a b
  | b < 0 = Left $ CmdFailed Exp "Negative exponent"
  | otherwise = return $ a ^ b
