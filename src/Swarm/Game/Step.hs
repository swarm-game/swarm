-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Swarm.Game.Step
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Facilities for stepping the robot CEK machines, /i.e./ the actual
-- interpreter for the Swarm language.
module Swarm.Game.Step where

import Control.Arrow ((***))
import Control.Lens hiding (Const, from, parts)
import Control.Monad.Except
import Control.Monad.State
import Data.Bool (bool)
import Data.Either (rights)
import Data.Int (Int64)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Linear
import System.Random (randomRIO)
import Witch
import Prelude hiding (lookup)

import Swarm.Game.CEK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton)
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

-- | The maximum number of CEK machine evaluation steps each robot is
--   allowed during a single game tick.
evalStepsPerTick :: Int
evalStepsPerTick = 100

-- | The main function to do one game tick.  The only reason we need
--   @IO@ is so that robots can run programs loaded from files, via
--   the 'Run' command; but eventually I want to get rid of that
--   command and have a library of modules that you can create, edit,
--   and run all from within the UI (the library could also be loaded
--   from a file when the whole program starts up).
gameTick :: (MonadState GameState m, MonadIO m) => m ()
gameTick = do
  wakeUpRobotsDoneSleeping
  robotNames <- use activeRobots
  time <- use ticks
  forM_ robotNames $ \rn -> do
    mr <- uses robotMap (M.lookup rn)
    case mr of
      Nothing -> return ()
      Just curRobot -> do
        curRobot' <- tickRobot time curRobot
        case curRobot' ^. selfDestruct of
          True -> deleteRobot rn
          False -> robotMap %= M.insert rn curRobot'
        mapM_ (sleepUntil rn) (waitingUntil curRobot')

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
flagRedraw :: MonadState GameState m => ExceptT Exn (StateT s m) ()
flagRedraw = lift . lift $ needsRedraw .= True

-- | Perform an action requiring a 'W.World' state component in a
--   larger context with a 'GameState'.
zoomWorld :: MonadState GameState m => (forall n. MonadState (W.World Int Entity) n => n a) -> m a
zoomWorld n = do
  w <- use world
  (a, w') <- runStateT n w
  world .= w'
  return a

-- | Get the entity (if any) at a given location.
entityAt :: MonadState GameState m => V2 Int64 -> ExceptT Exn (StateT Robot m) (Maybe Entity)
entityAt loc = lift . lift $ zoomWorld (W.lookupEntityM (W.locToCoords loc))

-- | Modify the entity (if any) at a given location.
updateEntityAt ::
  MonadState GameState m =>
  V2 Int64 ->
  (Maybe Entity -> Maybe Entity) ->
  ExceptT Exn (StateT Robot m) ()
updateEntityAt loc upd = lift . lift $ zoomWorld (W.updateM (W.locToCoords loc) upd)

-- | Get the robot with a given name (if any).
robotNamed :: MonadState GameState m => Text -> ExceptT Exn (StateT Robot m) (Maybe Robot)
robotNamed nm = lift . lift $ use (robotMap . at nm)

-- | Manhattan distance between world locations.
manhattan :: V2 Int64 -> V2 Int64 -> Int64
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

------------------------------------------------------------
-- Exceptions and validation
------------------------------------------------------------

-- | Ensure that a robot is capable of executing a certain constant
--   (either because it has a device which gives it that capability,
--   or it is a system robot, or we are in creative mode).
ensureCanExecute :: MonadState GameState m => Const -> ExceptT Exn (StateT Robot m) ()
ensureCanExecute c = do
  mode <- lift . lift $ use gameMode
  sys <- use systemRobot
  robotCaps <- use robotCapabilities
  let missingCaps = constCaps c `S.difference` robotCaps
  (sys || mode == Creative || S.null missingCaps)
    `holdsOr` Incapable missingCaps (TConst c)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityOr :: MonadState GameState m => Capability -> Exn -> ExceptT Exn (StateT Robot m) ()
hasCapabilityOr cap exn = do
  mode <- lift . lift $ use gameMode
  sys <- use systemRobot
  caps <- use robotCapabilities
  (sys || mode == Creative || cap `S.member` caps) `holdsOr` exn

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts)

-- | Raise an exception about a command failing with a formatted error message.
raise :: MonadState GameState m => Const -> [Text] -> ExceptT Exn (StateT Robot m) a
raise c parts = throwError (cmdExn c parts)

-- | Run a subcomputation that might throw an exception in a context
--   where we are returning a CEK machine; any exception will be
--   turned into an 'Up' state.
withExceptions :: Monad m => Cont -> ExceptT Exn (StateT Robot m) CEK -> StateT Robot m CEK
withExceptions k m = do
  res <- runExceptT m
  case res of
    Left exn -> return $ Up exn k
    Right a -> return a

------------------------------------------------------------
-- Stepping robots
------------------------------------------------------------

-- | Run a robot for one tick, which may consist of up to
--   'evalStepsPerTick' CEK machine steps and at most one command
--   execution.
tickRobot :: (MonadState GameState m, MonadIO m) => Integer -> Robot -> m Robot
tickRobot time = tickRobotRec time . (tickSteps .~ evalStepsPerTick)

-- | Recursive helper function for 'tickRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
tickRobotRec :: (MonadState GameState m, MonadIO m) => Integer -> Robot -> m Robot
tickRobotRec time r
  | not (isActive r) || r ^. tickSteps <= 0 = return r
  | otherwise = stepRobot time r >>= tickRobotRec time

-- | Single-step a robot by decrementing its 'tickSteps' counter and
--   running its CEK machine for one step.
stepRobot :: (MonadState GameState m, MonadIO m) => Integer -> Robot -> m Robot
stepRobot time r = do
  (cek', r') <- runStateT (stepCEK time (r ^. machine)) (r & tickSteps -~ 1)
  return $ r' & machine .~ cek'

-- | The main CEK machine workhorse.  Given a robot, look at its CEK
--   machine state and figure out a single next step.
stepCEK :: (MonadState GameState m, MonadIO m) => Integer -> CEK -> StateT Robot m CEK
stepCEK time cek = case cek of
  -- (liftIO $ appendFile "out.txt" (prettyCEK cek)) >>

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
  -- then stepCEK is a no-op.
  Waiting wakeupTime cek'
    | wakeupTime == time -> stepCEK time cek'
    | otherwise -> return cek
  -- Now some straightforward cases.  These all immediately turn
  -- into values.
  In TUnit _ k -> return $ Out VUnit k
  In (TDir d) _ k -> return $ Out (VDir d) k
  In (TInt n) _ k -> return $ Out (VInt n) k
  In (TString s) _ k -> return $ Out (VString s) k
  In (TBool b) _ k -> return $ Out (VBool b) k
  -- There should not be any antiquoted variables left at this point.
  In (TAntiString v) _ k ->
    return $ Up (Fatal (T.append "Antiquoted variable found at runtime: $str:" v)) k
  In (TAntiInt v) _ k ->
    return $ Up (Fatal (T.append "Antiquoted variable found at runtime: $int:" v)) k
  -- A constant is turned into a VCApp which might be waiting for arguments.
  In (TConst c) _ k -> return $ Out (VCApp c []) k
  -- To evaluate a variable, just look it up in the context.
  In (TVar x) e k -> withExceptions k $ do
    v <-
      lookup x e
        `isJustOr` Fatal (T.unwords ["Undefined variable", x, "encountered while running the interpreter."])
    return $ Out v k

  -- To evaluate a pair, start evaluating the first component.
  In (TPair t1 t2) e k -> return $ In t1 e (FSnd t2 e : k)
  -- Once that's done, evaluate the second component.
  Out v1 (FSnd t2 e : k) -> return $ In t2 e (FFst v1 : k)
  -- Finally, put the results together into a pair value.
  Out v2 (FFst v1 : k) -> return $ Out (VPair v1 v2) k
  -- Lambdas immediately turn into closures.
  In (TLam x _ t) e k -> return $ Out (VClo x t e) k
  -- To evaluate an application, start by focusing on the left-hand
  -- side and saving the argument for later.
  In (TApp t1 t2) e k -> return $ In t1 e (FArg t2 e : k)
  -- Once that's done, switch to evaluating the argument.
  Out v1 (FArg t2 e : k) -> return $ In t2 e (FApp v1 : k)
  -- We can evaluate an application of a closure in the usual way.
  Out v2 (FApp (VClo x t e) : k) -> return $ In t (addBinding x v2 e) k
  -- We can also evaluate an application of a constant by collecting
  -- arguments, eventually dispatching to evalConst for function
  -- constants.
  Out v2 (FApp (VCApp c args) : k)
    | not (isCmd c)
        && arity c == length args + 1 ->
      evalConst time c (reverse (v2 : args)) k
    | otherwise -> return $ Out (VCApp c (v2 : args)) k
  Out _ (FApp _ : _) -> badMachineState "FApp of non-function"
  -- To evaluate let expressions, we start by focusing on the
  -- let-bound expression. Since it can be recursive, we wrap it in
  -- @VDelay@ (the elaboration step wrapped all recursive references
  -- in a corresponding @Force@).
  In (TLet x _ t1 t2) e k ->
    let e' = addBinding x (VDelay (Just x) t1 e) e
     in return $ In t1 e' (FLet x t2 e : k)
  -- Once we've finished with the let-binding, we switch to evaluating
  -- the body in a suitably extended environment.
  Out v1 (FLet x t2 e : k) -> return $ In t2 (addBinding x v1 e) k
  -- Definitions immediately turn into VDef values, awaiting execution.
  In tm@(TDef x _ t) e k -> withExceptions k $ do
    CEnv `hasCapabilityOr` Incapable (S.singleton CEnv) tm
    return $ Out (VDef x t e) k

  -- Bind expressions don't evaluate: just package it up as a value
  -- until such time as it is to be executed.
  In (TBind mx t1 t2) e k -> return $ Out (VBind mx t1 t2 e) k
  -- Delay expressions immediately turn into VDelay values, awaiting
  -- application of 'Force'.
  In (TDelay t) e k -> return $ Out (VDelay Nothing t e) k
  ------------------------------------------------------------
  -- Execution

  -- To execute a definition, we immediately turn the body into a
  -- delayed value, so it will not even be evaluated until it is
  -- called.  We return a special VResult value, which packages up the
  -- return value from the @def@ command itself (@unit@) together with
  -- the resulting environment (the variable bound to the delayed
  -- value).
  Out (VDef x t e) (FExec : k) -> do
    return $ Out (VResult VUnit (singleton x (VDelay (Just x) t e))) k

  -- To execute a constant application, delegate to the 'execConst'
  -- function.  Set tickSteps to 0 if the command is supposed to take
  -- a tick, so the robot won't take any more steps this tick.
  Out (VCApp c args) (FExec : k) -> do
    when (takesTick c) $ tickSteps .= 0
    res <- runExceptT (execConst time c (reverse args) k)
    case res of
      Left exn -> return $ Up exn k
      Right cek' -> return cek'

  -- To execute a bind expression, evaluate and execute the first
  -- command, and remember the second for execution later.
  Out (VBind mx c1 c2 e) (FExec : k) -> return $ In c1 e (FExec : FBind mx c2 e : k)
  -- If first command completes with a value along with an environment
  -- resulting from definition commands, switch to evaluating the
  -- second command of the bind.  Extend the environment with both the
  -- definition environment resulting from the first command, as well
  -- as a binding for the result (if the bind was of the form @x <-
  -- c1; c2@).  Remember that we must execute the second command once
  -- it has been evaluated, then union any resulting definition
  -- environment with the definition environment from the first
  -- command.
  Out (VResult v ve) (FBind mx t2 e : k) ->
    return $ In t2 (maybe id (`addBinding` v) mx . (`union` ve) $ e) (FExec : FUnionEnv ve : k)
  -- On the other hand, if the first command completes with a simple value,
  -- we do something similar, but don't have to worry about the environment.
  Out v (FBind mx t2 e : k) ->
    return $ In t2 (maybe id (`addBinding` v) mx e) (FExec : k)
  -- If a command completes with a value and definition environment,
  -- and the next continuation frame contains a previous environment
  -- to union with, then pass the unioned environments along in
  -- another VResult.

  Out (VResult v e2) (FUnionEnv e1 : k) -> return $ Out (VResult v (e1 `union` e2)) k
  -- Or, if a command completes with no environment, but there is a
  -- previous environment to union with, just use that environment.
  Out v (FUnionEnv e : k) -> return $ Out (VResult v e) k
  -- If the top of the continuation stack contains a 'FLoadEnv' frame,
  -- it means we are supposed to load up the resulting definition
  -- environment and type and capability contexts into the robot's
  -- top-level environment and contexts, so they will be available to
  -- future programs.
  Out (VResult v e) (FLoadEnv ctx cctx : k) -> do
    robotEnv %= (`union` e)
    robotCtx %= ((`union` ctx) *** (`union` cctx))
    return $ Out v k
  Out v (FLoadEnv {} : k) -> return $ Out v k
  -- Any other type of value wiwth an FExec frame is an error (should
  -- never happen).
  Out _ (FExec : _) -> badMachineState "FExec frame with non-executable value"
  -- Any other frame with a VResult is an error (should never happen).
  Out (VResult _ _) _ -> badMachineState "no appropriate stack frame to catch a VResult"
  ------------------------------------------------------------
  -- Exception handling
  ------------------------------------------------------------

  -- First, if we were running a try block but evaluation completed normally,
  -- just ignore the try block and continue.
  Out v (FTry _ : k) -> return $ Out v k
  -- If an exception rises all the way to the top level without being
  -- handled, turn it into an error message via the 'say' command.
  -- Note that (for now at least) the 'say' command requires no
  -- capabilities, so this cannot generate an infinite loop. However,
  -- in the future we might differentiate between a 'log' command
  -- (which appends to an internal message queue, visible only via
  -- 'view'), and might require a logging device, and a 'say' command
  -- which broadcasts to other (nearby?) robots.

  -- NOTE, if this is changed to go via e.g. a Log command that
  -- requires a capability, make sure to check for that capability
  -- here and silently discard the message if the robot can't do
  -- logging!  Otherwise trying to exceute the Log command will
  -- generate another exception, which will be logged, which will
  -- generate an exception, ... etc.
  Up exn [] -> return $ In (TApp (TConst Say) (TString (formatExn exn))) empty [FExec]
  -- Fatal errors and capability errors can't be caught; just throw
  -- away the continuation stack.
  Up exn@Fatal {} _ -> return $ Up exn []
  Up exn@Incapable {} _ -> return $ Up exn []
  -- Otherwise, if we are raising an exception up the continuation
  -- stack and come to a Try frame, execute the associated catch
  -- block.
  Up _ (FTry c : k) -> return $ Out c (FExec : k)
  -- Otherwise, keep popping from the continuation stack.
  Up exn (_ : k) -> return $ Up exn k
  -- Finally, if we're done evaluating and the continuation stack is
  -- empty, return the machine unchanged.
  done@(Out _ []) -> return done
 where
  badMachineState msg =
    let msg' =
          T.unlines
            [ T.append "Bad machine state in stepRobot: " msg
            , from (prettyCEK cek)
            ]
     in return $ Up (Fatal msg') []

-- | Determine whether a constant should take up a tick or not when executed.
takesTick :: Const -> Bool
takesTick c = isCmd c && (c `notElem` [Selfdestruct, Noop, Return, GetX, GetY, Blocked, Ishere, Try, Random, Appear])

-- | At the level of the CEK machine, the only difference bewteen
--   between *evaluating* a function constant and *executing* a
--   command constant is what kind of exceptions can be thrown.  When
--   evaluating, the only thing that could throw an exception is
--   trying to use a function constant without the proper capability
--   (for example, trying to use `if` without having a conditional
--   device).  Any other exceptions constitute a bug.
evalConst :: (MonadState GameState m, MonadIO m) => Integer -> Const -> [Value] -> Cont -> StateT Robot m CEK
evalConst time c vs k = do
  res <- runExceptT $ execConst time c vs k
  case res of
    Left exn@Fatal {} -> return $ Up exn k
    Left exn@Incapable {} -> return $ Up exn k
    Left exn -> do
      let msg =
            T.unlines
              [ "evalConst shouldn't be able to throw this kind of exception:"
              , formatExn exn
              ]
      return $ Up (Fatal msg) k
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

-- | Interpret the execution (or evaluation) of a constant application
--   to some values.
execConst :: (MonadState GameState m, MonadIO m) => Integer -> Const -> [Value] -> Cont -> ExceptT Exn (StateT Robot m) CEK
execConst time c vs k = do
  -- First, ensure the robot is capable of executing/evaluating this constant.
  ensureCanExecute c

  -- Now proceed to actually carry out the operation.
  case c of
    Noop -> return $ Out VUnit k
    Return -> case vs of
      [v] -> return $ Out v k
      _ -> badConst
    Wait -> case vs of
      [VInt d] -> return $ Waiting (time + d) (Out VUnit k)
      _ -> badConst
    Selfdestruct -> do
      selfDestruct .= True
      flagRedraw
      return $ Out VUnit k
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
            `holdsOr` cmdExn Move ["There is a", e ^. entityName, "in the way!"]

          -- Robots drown if they walk over liquid
          caps <- use robotCapabilities
          when (e `hasProperty` Liquid && CFloat `S.notMember` caps) $
            selfDestruct .= True

      robotLocation .= nextLoc
      flagRedraw
      return $ Out VUnit k
    Grab -> do
      -- Ensure there is an entity here.
      loc <- use robotLocation
      e <- entityAt loc >>= (`isJustOr` cmdExn Grab ["There is nothing here to grab."])

      -- Ensure it can be picked up.
      (e `hasProperty` Portable)
        `holdsOr` cmdExn Grab ["The", e ^. entityName, "here can't be grabbed."]

      -- Remove the entity from the world.
      updateEntityAt loc (const Nothing)
      flagRedraw

      -- Possibly regrow the entity.
      when (e `hasProperty` Growable) $ do
        let GrowthTime (minT, maxT) = (e ^. entityGrowth) ? defaultGrowthTime

        case maxT of
          -- Special case: if the growth time is zero, just add the
          -- entity back immediately.
          0 -> updateEntityAt loc (const (Just e))
          -- Otherwise, grow a new entity from a seed.
          _ -> do
            let seedBot =
                  mkRobot
                    "seed"
                    loc
                    (V2 0 0)
                    (initMachine (seedProgram minT (maxT - minT) (e ^. entityName)) empty)
                    []
                    & robotDisplay
                      .~ (defaultEntityDisplay '.' & displayAttr .~ (e ^. entityDisplay . displayAttr))
                    & robotInventory .~ E.singleton e
                    & systemRobot .~ True
            _ <- lift . lift $ addRobot seedBot
            return ()

      -- Add the picked up item to the robot's inventory.  If the
      -- entity yields something different, add that instead.
      let yieldName = e ^. entityYields
      e' <- case yieldName of
        Nothing -> return e
        Just n -> (?e) <$> (lift . lift $ uses entityMap (lookupEntityName n))

      robotInventory %= insert e'

      -- Return the name of the item grabbed.
      return $ Out (VString (e ^. entityName)) k
    Turn -> case vs of
      [VDir d] -> do
        -- "treads" `isInstalledOr` cmdExn Turn ["You need treads to turn."]

        robotOrientation . _Just %= applyTurn d
        flagRedraw
        return $ Out VUnit k
      _ -> badConst
    Place -> case vs of
      [VString s] -> do
        inv <- use robotInventory
        loc <- use robotLocation

        -- Make sure there's nothing already here
        nothingHere <- isNothing <$> entityAt loc
        nothingHere `holdsOr` cmdExn Place ["There is already an entity here."]

        -- Make sure the robot has the thing in its inventory
        e <-
          listToMaybe (lookupByName s inv)
            `isJustOr` cmdExn Place ["You don't have", indefinite s, "to place."]

        -- Place the entity and remove it from the inventory
        updateEntityAt loc (const (Just e))
        robotInventory %= delete e

        flagRedraw
        return $ Out VUnit k
      _ -> badConst
    Give -> case vs of
      [VString otherName, VString itemName] -> do
        -- Make sure the other robot exists
        other <-
          robotNamed otherName
            >>= (`isJustOr` cmdExn Give ["There is no robot named", otherName, "."])

        -- Make sure it is in the same location
        loc <- use robotLocation
        ((other ^. robotLocation) `manhattan` loc <= 1)
          `holdsOr` cmdExn Give ["The robot named", otherName, "is not close enough."]

        -- Make sure we have the required item
        inv <- use robotInventory
        item <-
          (listToMaybe . lookupByName itemName $ inv)
            `isJustOr` cmdExn Give ["You don't have", indefinite itemName, "to give."]

        -- Giving something to ourself should be a no-op.  We need
        -- this as a special case since it will not work to modify
        -- ourselves in the robotMap --- after performing a tick we
        -- return a modified Robot which gets put back in the
        -- robotMap, overwriting any changes to this robot made
        -- directly in the robotMap during the tick.
        myName <- use robotName
        focusedName <- lift . lift $ use focusedRobotName
        when (otherName /= myName) $ do
          -- Make the exchange
          lift . lift $ robotMap . at otherName . _Just . robotInventory %= insert item
          robotInventory %= delete item

          -- Flag the UI for a redraw if we are currently showing either robot's inventory
          when (focusedName == myName || focusedName == otherName) flagRedraw

        return $ Out VUnit k
      _ -> badConst
    Install -> case vs of
      [VString otherName, VString itemName] -> do
        -- Make sure the other robot exists
        other <-
          robotNamed otherName
            >>= (`isJustOr` cmdExn Install ["There is no robot named", otherName, "."])

        -- Make sure it is in the same location
        loc <- use robotLocation
        ((other ^. robotLocation) `manhattan` loc <= 1)
          `holdsOr` cmdExn Install ["The robot named", otherName, "is not close enough."]

        -- Make sure we have the required item
        inv <- use robotInventory
        item <-
          (listToMaybe . lookupByName itemName $ inv)
            `isJustOr` cmdExn Install ["You don't have", indefinite itemName, "to install."]

        myName <- use robotName
        focusedName <- lift . lift $ use focusedRobotName
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
            already <- lift . lift $ preuse (otherDevices . to (`E.contains` item))
            unless (already == Just True) $ do
              lift . lift $ robotMap . at otherName . _Just . installedDevices %= insert item
              robotInventory %= delete item

              -- Flag the UI for a redraw if we are currently showing
              -- either robot's inventory
              when (focusedName == myName || focusedName == otherName) flagRedraw

        return $ Out VUnit k
      _ -> badConst
    Make -> case vs of
      [VString name] -> do
        inv <- use robotInventory
        em <- lift . lift $ use entityMap
        e <-
          lookupEntityName name em
            `isJustOr` cmdExn Make ["I've never heard of", indefiniteQ name, "."]

        outRs <- lift . lift $ use recipesOut

        -- Only consider recipes where the number of things we are trying to make
        -- is greater in the outputs than in the inputs.  This prevents us from doing
        -- silly things like making copper pipes when the user says "make furnace".
        let recipes = filter increase (recipesFor outRs e)
            increase (Recipe ins outs _) = countIn outs > countIn ins
            countIn xs = maybe 0 fst (find ((== e) . snd) xs)
        not (null recipes)
          `holdsOr` cmdExn Make ["There is no known recipe for making", indefinite name, "."]

        -- Now try each recipe and take the first one that we have the
        -- ingredients for.
        inv' <-
          listToMaybe (rights (map (make inv) recipes))
            `isJustOr` cmdExn Make ["You don't have the ingredients to make", indefinite name, "."]

        robotInventory .= inv'
        return $ Out VUnit k
      _ -> badConst
    GetX -> do
      V2 x _ <- use robotLocation
      return $ Out (VInt (fromIntegral x)) k
    GetY -> do
      V2 _ y <- use robotLocation
      return $ Out (VInt (fromIntegral y)) k
    Blocked -> do
      loc <- use robotLocation
      orient <- use robotOrientation
      let nextLoc = loc ^+^ (orient ? zero)
      me <- entityAt nextLoc
      return $ Out (VBool (maybe False (`hasProperty` Unwalkable) me)) k
    Scan -> case vs of
      [VDir d] -> do
        loc <- use robotLocation
        orient <- use robotOrientation
        let scanLoc = loc ^+^ applyTurn d (orient ? zero)
        me <- entityAt scanLoc
        case me of
          Nothing -> return ()
          Just e -> robotInventory %= insertCount 0 e

        return $ Out VUnit k
      _ -> badConst
    Upload -> case vs of
      [VString otherName] -> do
        -- Make sure the other robot exists
        other <-
          robotNamed otherName
            >>= (`isJustOr` cmdExn Upload ["There is no robot named", otherName, "."])

        -- Make sure it is in the same location
        loc <- use robotLocation
        ((other ^. robotLocation) `manhattan` loc <= 1)
          `holdsOr` cmdExn Upload ["The robot named", otherName, "is not close enough."]

        -- Upload knowledge of everything in our inventory
        inv <- use robotInventory
        forM_ (elems inv) $ \(_, e) ->
          lift . lift $ robotMap . at otherName . _Just . robotInventory %= insertCount 0 e

        return $ Out VUnit k
      _ -> badConst
    Random -> case vs of
      [VInt hi] -> do
        n <- randomRIO (0, hi -1)
        return $ Out (VInt n) k
      _ -> badConst
    Say -> case vs of
      [VString s] -> do
        rn <- use robotName
        lift . lift $ emitMessage (T.concat [rn, ": ", s])
        return $ Out VUnit k
      _ -> badConst
    View -> case vs of
      [VString s] -> do
        _ <-
          robotNamed s
            >>= (`isJustOr` cmdExn View ["There is no robot named ", s, " to view."])

        -- Only the base can actually change the view in the UI.  Other robots can
        -- execute this command but it does nothing (at least for now).
        rn <- use robotName
        when (rn == "base") $
          lift . lift $ viewCenterRule .= VCRobot s

        return $ Out VUnit k
      _ -> badConst
    Appear -> case vs of
      [VString s] -> do
        flagRedraw
        case into @String s of
          [dc] -> do
            robotDisplay . defaultChar .= dc
            robotDisplay . orientationMap .= M.empty
            return $ Out VUnit k
          [dc, nc, ec, sc, wc] -> do
            robotDisplay . defaultChar .= dc
            robotDisplay . orientationMap . ix North .= nc
            robotDisplay . orientationMap . ix East .= ec
            robotDisplay . orientationMap . ix South .= sc
            robotDisplay . orientationMap . ix West .= wc
            return $ Out VUnit k
          _other -> raise Appear [quote s, "is not a valid appearance string."]
      _ -> badConst
    Create -> case vs of
      [VString name] -> do
        em <- lift . lift $ use entityMap
        e <-
          lookupEntityName name em
            `isJustOr` cmdExn Create ["I've never heard of", indefiniteQ name, "."]

        robotInventory %= insert e
        return $ Out VUnit k
      _ -> badConst
    Ishere -> case vs of
      [VString s] -> do
        loc <- use robotLocation
        me <- entityAt loc
        case me of
          Nothing -> return $ Out (VBool False) k
          Just e -> return $ Out (VBool (T.toLower (e ^. entityName) == T.toLower s)) k
      _ -> badConst
    Force -> case vs of
      [VDelay Nothing t e] -> return $ In t e k
      [VDelay (Just x) t e] -> return $ In t (addBinding x (VDelay (Just x) t e) e) k
      _ -> badConst
    -- Note, if should evaluate the branches lazily, but since
    -- evaluation is eager, by the time we get here thn and els have
    -- already been fully evaluated --- what gives?  The answer is that
    -- we rely on elaboration to add 'lazy' wrappers around the branches
    -- (and a 'force' wrapper around the entire if).
    If -> case vs of
      [VBool b, thn, els] -> return $ Out (bool els thn b) k
      _ -> badConst
    Fst -> case vs of
      [VPair v _] -> return $ Out v k
      _ -> badConst
    Snd -> case vs of
      [VPair _ v] -> return $ Out v k
      _ -> badConst
    Try -> case vs of
      [c1, c2] -> return $ Out c1 (FExec : FTry c2 : k)
      _ -> badConst
    Raise -> case vs of
      [VString s] -> return $ Up (User s) k
      _ -> badConst
    Build -> case vs of
      [VString name, VDelay _ cmd e] -> do
        r <- get
        em <- lift . lift $ use entityMap
        mode <- lift . lift $ use gameMode

        let -- Standard devices that are always installed.
            -- XXX in the future, make a way to build these and just start the base
            -- out with a large supply of each?
            stdDeviceList = ["treads", "grabber", "solar panel", "detonator", "scanner"]
            stdDevices = S.fromList $ mapMaybe (`lookupEntityName` em) stdDeviceList

            -- Find out what capabilities are required by the program that will
            -- be run on the newly constructed robot, and what devices would
            -- provide those capabilities.
            (caps, _capCtx) = requiredCaps (snd (r ^. robotCtx)) cmd
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
          `holdsOr` cmdExn
            Build
            [ "this would require installing devices you don't have:\n"
            , commaList (map (^. entityName) (S.toList missingDevices))
            ]

        -- Construct the new robot.
        let newRobot =
              mkRobot
                name
                (r ^. robotLocation)
                (r ^. robotOrientation ? east)
                (In cmd e [FExec])
                (S.toList devices)

        -- Add the new robot to the world.
        newRobot' <- lift . lift $ addRobot newRobot

        -- Remove from the inventory any devices which were installed on the new robot,
        -- if not in creative mode.
        unless (mode == Creative) $
          forM_ (devices `S.difference` stdDevices) $ \d ->
            robotInventory %= delete d

        -- Flag the world for a redraw and return the name of the newly constructed robot.
        flagRedraw
        return $ Out (VString (newRobot' ^. robotName)) k
      _ -> badConst
    -- run can take both types of text inputs
    -- with and without file extension as in
    -- "./path/to/file.sw" and "./path/to/file"
    Run -> case vs of
      [VString fileName] -> do
        mf <- liftIO $ mapM readFileMay [into fileName, into $ fileName <> ".sw"]

        f <- msum mf `isJustOr` cmdExn Run ["File not found:", fileName]

        t <-
          processTerm (into @Text f) `isRightOr` \err ->
            cmdExn Run ["Error in", fileName, "\n", err]

        return $ initMachine' t empty k
      _ -> badConst
    Not -> case vs of
      [VBool b] -> return $ Out (VBool (not b)) k
      _ -> badConst
    Neg -> case vs of
      [VInt n] -> return $ Out (VInt (- n)) k
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
 where
  badConst =
    throwError $
      Fatal $
        T.unlines
          [ "Bad application of execConst:"
          , from (prettyCEK (Out (VCApp c (reverse vs)) k))
          ]
  returnEvalCmp = case vs of
    [v1, v2] ->
      case evalCmp c v1 v2 of
        Nothing -> return $ Out (VBool False) k
        Just b -> return $ Out (VBool b) k
    _ -> badConst
  returnEvalArith = case vs of
    [VInt n1, VInt n2] -> return $ Out (VInt $ evalArith c n1 n2) k
    _ -> badConst

-- | Evaluate the application of a comparison operator.  Returns
--   @Nothing@ if the application does not make sense.
evalCmp :: Const -> Value -> Value -> Maybe Bool
evalCmp c v1 v2 = decideCmp c $ compareValues v1 v2
 where
  decideCmp = \case
    Eq -> fmap (== EQ)
    Neq -> fmap (/= EQ)
    Lt -> fmap (== LT)
    Gt -> fmap (== GT)
    Leq -> fmap (/= GT)
    Geq -> fmap (/= LT)
    _ -> const Nothing

-- | Compare two values, returning an 'Ordering' if they can be
--   compared, or @Nothing@ if they cannot.
compareValues :: Value -> Value -> Maybe Ordering
compareValues = \case
  VUnit -> \case VUnit -> Just EQ; _ -> Nothing
  VInt n1 -> \case VInt n2 -> Just (compare n1 n2); _ -> Nothing
  VString t1 -> \case VString t2 -> Just (compare t1 t2); _ -> Nothing
  VDir d1 -> \case VDir d2 -> Just (compare d1 d2); _ -> Nothing
  VBool b1 -> \case VBool b2 -> Just (compare b1 b2); _ -> Nothing
  VPair v11 v12 -> \case
    VPair v21 v22 ->
      (<>) <$> compareValues v11 v21 <*> compareValues v12 v22
    _ -> Nothing
  VClo {} -> const Nothing
  VCApp {} -> const Nothing
  VDef {} -> const Nothing
  VResult {} -> const Nothing
  VBind {} -> const Nothing
  VDelay {} -> const Nothing

-- | Evaluate the application of an arithmetic operator.  Note, we
--   want to maintain the invariant that only executing commands can
--   throw exceptions, not evaluating pure expressions; hence,
--   dividing by zero and exponentiating by a negative number have to
--   return some value even though they aren't sensible.  At the
--   moment, they return 42.  In the future it might be fun if they
--   return some kind of random result.
--   In case we incorrectly use it on bad Const in the library return
--   huge value.
evalArith :: Const -> Integer -> Integer -> Integer
evalArith = \case
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> safeDiv
  Exp -> safeExp
  _ -> (\_ _ -> 0xbadc0de)

-- | Perform an integer division, but return 42 for division by zero.
safeDiv :: Integer -> Integer -> Integer
safeDiv _ 0 = 42
safeDiv a b = a `div` b

-- | Perform exponentiation, but return 42 if the power is negative.
safeExp :: Integer -> Integer -> Integer
safeExp a b
  | b < 0 = 42
  | otherwise = a ^ b
