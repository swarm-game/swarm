-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Step
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Facilities for stepping the robot CEK machines, /i.e./ the actual
-- interpreter for the Swarm language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Swarm.Game.Step where

import           Control.Arrow           ((&&&))
import           Control.Lens            hiding (Const, from, parts)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map                as M
import           Data.Maybe              (fromJust, isNothing, listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Linear
import           System.Random           (randomRIO)
import           Witch

import           Swarm.Game.CEK
import           Swarm.Game.Display
import           Swarm.Game.Entities     as E
import           Swarm.Game.Entity       as E
import           Swarm.Game.Exception    (formatExn)
import           Swarm.Game.Recipes
import           Swarm.Game.Robot
import           Swarm.Game.State
import           Swarm.Game.Value        as V
import qualified Swarm.Game.World        as W
import           Swarm.Language.Pipeline
import           Swarm.Language.Syntax
import           Swarm.Language.Types
import           Swarm.TUI.Attr
import           Swarm.Util

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
gameStep :: (MonadState GameState m, MonadIO m) => m ()
gameStep = do

  -- Reset the updated flag.  While stepping the robots, the flag will
  -- get set to true if anything changes that requires redrawing the
  -- world (e.g. a robot moving or disappearing).
  updated .= False

  -- Note, it is tempting to do the below in one line with some clever
  -- lens combinator, but it's not possible.  We want to do an
  -- effectful traversal over a piece of the state (i.e. step each
  -- robot in the robot map, where stepping a robot could have effects
  -- on the game state), but the problem is the effects could in
  -- theory include modifying the very state we are traversing over.
  -- In fact, stepping one robot can even have effects on other robots
  -- (e.g. if one robot gives an item to another), so we can't even
  -- use something like M.traverseMaybeWithKey --- we have to get the
  -- names of all robots and then step them one at a time.
  robotNames <- uses robotMap M.keys
  forM_ robotNames $ \rn -> do
    mr <- uses robotMap (M.lookup rn)
    case mr of
      Nothing -> return ()
      Just curRobot -> do
        curRobot' <- bigStepRobot curRobot
        case curRobot' ^. selfDestruct of
          True  -> robotMap %= M.delete rn
          False -> robotMap %= M.insert rn curRobot'

  -- See if the base is finished with a computation, and if so, record
  -- the result in the game state so it can be displayed by the REPL.
  mr <- use (robotMap . at "base")
  case mr of
    Just r  -> do
      res <- use replResult
      case res of
        REPLWorking ty Nothing -> replResult .= REPLWorking ty (getResult r)
        _                      -> return ()
    Nothing -> return ()

  -- Get all the newly built robots and add them to the robot map.
  new <- use newRobots
  robotMap %= M.union (M.fromList $ map (view robotName &&& id) new)
  newRobots .= []

  -- Possible update the view center.
  modify updateViewCenter

------------------------------------------------------------
-- Some utility functions
------------------------------------------------------------

-- | Set a flag telling the UI that the world needs to be redrawn.
flagRedraw :: MonadState GameState m => ExceptT Exn (StateT s m) ()
flagRedraw = lift . lift $ updated .= True

-- | Get the entity (if any) at a given location.
entityAt :: MonadState GameState m => V2 Int -> ExceptT Exn (StateT Robot m) (Maybe Entity)
entityAt (V2 x y) = lift . lift $ uses world (W.lookupEntity (-y,x))

-- | Modify the entity (if any) at a given location.
updateEntityAt
  :: MonadState GameState m
  => V2 Int -> (Maybe Entity -> Maybe Entity) -> ExceptT Exn (StateT Robot m) ()
updateEntityAt (V2 x y) upd = lift . lift $ world %= W.update (-y,x) upd

-- | Get the robot with a given name (if any).
robotNamed :: MonadState GameState m => Text -> ExceptT Exn (StateT Robot m) (Maybe Robot)
robotNamed nm = lift . lift $ use (robotMap . at nm)

-- | Require that a Boolean value is @True@, or throw an exception.
holdsOr :: MonadError e m => Bool -> e -> m ()
holdsOr b e = unless b $ throwError e

-- | Require that a 'Maybe' value is 'Just', or throw an exception.
isJustOr :: MonadError e m => Maybe a -> e -> m a
Just a  `isJustOr` _ = return a
Nothing `isJustOr` e = throwError e

isRightOr :: MonadError e m => Either b a -> (b -> e) -> m a
Right a `isRightOr` _ = return a
Left b `isRightOr` f  = throwError (f b)

-- | Require that a robot has a given device installed, OR we are in
--   creative mode.
isInstalledOr :: MonadState GameState m => Entity -> Exn -> ExceptT Exn (StateT Robot m) ()
isInstalledOr ent exn = do
  mode <- lift . lift $ use gameMode
  r <- get
  unless (mode == Creative || r `hasInstalled` ent) $ throwError exn

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts)

-- | Raise an exception about a command failing with a formatted error message.
raise :: MonadState GameState m => Const -> [Text] -> ExceptT Exn (StateT Robot m) a
raise c parts = throwError (cmdExn c parts)

------------------------------------------------------------
-- Stepping robots
------------------------------------------------------------

-- | Run a robot for one "big step", which may consist of up to
--   'evalStepsPerTick' CEK machine steps and at most one command
--   execution.
bigStepRobot :: (MonadState GameState m, MonadIO m) => Robot -> m Robot
bigStepRobot = bigStepRobotRec . (tickSteps .~ evalStepsPerTick)

-- | Recursive helper function for 'bigStepRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
bigStepRobotRec :: (MonadState GameState m, MonadIO m) => Robot -> m Robot
bigStepRobotRec r
  | not (isActive r) || r ^. tickSteps <= 0 = return r
  | otherwise           = stepRobot r >>= bigStepRobotRec

-- | Single-step a robot by decrementing its 'tickSteps' counter and
--   running its CEK machine for one step.
stepRobot :: (MonadState GameState m, MonadIO m) => Robot -> m Robot
stepRobot r = do
  (cek', r') <- runStateT (stepCEK (r ^. machine)) (r & tickSteps -~ 1)
  return $ r' & machine .~ cek'

-- | The main CEK machine workhorse.  Given a robot, look at its CEK
--   machine state and figure out a single next step.
stepCEK :: (MonadState GameState m, MonadIO m) => CEK -> StateT Robot m CEK
stepCEK cek = case cek of

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

  -- First some straightforward cases.  These all immediately turn
  -- into values.
  In TUnit _ k                      -> return $ Out VUnit k
  In (TDir d) _ k                   -> return $ Out (VDir d) k
  In (TInt n) _ k                   -> return $ Out (VInt n) k
  In (TString s) _ k                -> return $ Out (VString s) k
  In (TBool b) _ k                  -> return $ Out (VBool b) k

  -- A constant is turned into a VCApp which might be waiting for arguments.
  In (TConst c) _ k                 -> return $ Out (VCApp c []) k

  -- To evaluate a variable, just look it up in the context.
  In (TVar x) e k                   -> return $ Out (e !!! x) k

  -- To evaluate a pair, start evaluating the first component.
  In (TPair t1 t2) e k              -> return $ In t1 e (FSnd t2 e : k)
  -- Once that's done, evaluate the second component.
  Out v1 (FSnd t2 e : k)            -> return $ In t2 e (FFst v1 : k)
  -- Finally, put the results together into a pair value.
  Out v2 (FFst v1 : k)              -> return $ Out (VPair v1 v2) k

  -- Lambdas immediately turn into closures.
  In (TLam x _ t) e k               -> return $ Out (VClo x t e) k

  -- To evaluate an application, start by focusing on the left-hand
  -- side and saving the argument for later.
  In (TApp t1 t2) e k               -> return $ In t1 e (FArg t2 e : k)
  -- Once that's done, switch to evaluating the argument.
  Out v1 (FArg t2 e : k)            -> return $ In t2 e (FApp v1 : k)
  -- We can evaluate an application of a closure in the usual way.
  Out v2 (FApp (VClo x t e) : k)    -> return $ In t (addBinding x v2 e) k
  -- We can also evaluate an application of a constant by collecting
  -- arguments, eventually dispatching to evalConst for function
  -- constants.
  Out v2 (FApp (VCApp c args) : k)
    | not (isCmd c) &&
      arity c == length args + 1    -> evalConst c (reverse (v2 : args)) k
    | otherwise                     -> return $ Out (VCApp c (v2 : args)) k
  Out _ (FApp _ : k) -> do
    let msg = T.unlines
          [ "Bad machine state in stepRobot: FApp of non-function"
          , from (show cek)
          ]
    return $ Up (Fatal msg) k

  -- Evaluating let expressions is a little bit tricky. We start by
  -- focusing on the let-bound expression. But since it is allowed to
  -- be recursive, we have to set up a recursive environment in which
  -- to evaluate it.  Note how we wrap the expression in VDelay; the
  -- elaboration step wrapped all recursive references in a
  -- corresponding @Force@.
  In (TLet x _ t1 t2) e k           ->
    let e' = addBinding x (VDelay t1 e') e   -- XXX do this without making a recursive
                                             -- (hence unprintable) env?
    in return $ In t1 e' (FLet x t2 e : k)

  -- Once we've finished with the let-binding, we switch to evaluating
  -- the body in a suitably extended environment.
  Out v1 (FLet x t2 e : k)          -> return $ In t2 (addBinding x v1 e) k

  -- Definitions immediately turn into VDef values, awaiting execution.
  In (TDef x _ t) e k               -> return $ Out (VDef x t e) k

  -- Bind expressions don't evaluate: just package it up as a value
  -- until such time as it is to be executed.
  In (TBind mx t1 t2) e k           -> return $ Out (VBind mx t1 t2 e) k

  -- Delay expressions immediately turn into VDelay values, awaiting
  -- application of 'Force'.
  In (TDelay t) e k                  -> return $ Out (VDelay t e) k

  ------------------------------------------------------------
  -- Execution

  -- To execute a definition, we focus on evaluating the body in a
  -- recursive environment (similar to let), and remember that the
  -- result should be bound to the given name.
  Out (VDef x t e) (FExec : k)       ->
    let e' = addBinding x (VDelay t e') e
    in  return $ In t e' (FDef x : k)
  -- Once we are done evaluating the body of a definition, we return a
  -- special VResult value, which packages up the return value from
  -- the @def@ command itself (@unit@) together with the resulting
  -- environment (the variable bound to the value).
  Out v  (FDef x : k)                -> return $ Out (VResult VUnit (V.singleton x v)) k

  -- To execute a constant application, delegate to the 'execConst'
  -- function.  Set tickSteps to 0 if the command is supposed to take
  -- a tick, so the robot won't take any more steps this tick.
  Out (VCApp c args) (FExec : k)     -> do
    when (takesTick c) $ tickSteps .= 0
    res <- runExceptT (execConst c (reverse args) k)
    case res of
      Left exn   -> return $ Up exn k
      Right cek' -> return cek'

  -- To execute a bind expression, evaluate and execute the first
  -- command, and remember the second for execution later.
  Out (VBind mx c1 c2 e) (FExec : k)  -> return $ In c1 e (FExec : FBind mx c2 e : k)
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
    return $ In t2 (ve `V.union` maybe id (`addBinding` v) mx e) (FExec : FUnionEnv ve : k)
  -- On the other hand, if the first command completes with a simple value,
  -- we do something similar, but don't have to worry about the environment.
  Out v (FBind mx t2 e : k) ->
    return $ In t2 (maybe id (`addBinding` v) mx e) (FExec : k)
  -- If a command completes with a value and definition environment,
  -- and the next continuation frame contains a previous environment
  -- to union with, then pass the unioned environments along in
  -- another VResult.

  Out (VResult v e2) (FUnionEnv e1 : k) -> return $ Out (VResult v (e2 `V.union` e1)) k
  -- Or, if a command completes with no environment, but there is a
  -- previous environment to union with, just use that environment.
  Out v (FUnionEnv e : k)               -> return $ Out (VResult v e) k

  -- If the top of the continuation stack contains a 'FLoadEnv' frame,
  -- it means we are supposed to load up the resulting definition
  -- environment and type context into the robot's top-level
  -- environment and type context, so they will be available to future
  -- programs.
  Out (VResult v e) (FLoadEnv ctx : k)  -> do
    robotEnv %= V.union e
    robotCtx %= M.union ctx
    return $ Out v k
  Out v (FLoadEnv _ : k)                -> return $ Out v k

  -- Exception handling.

  -- First, if we were running a try block but evaluation completed normally,
  -- just ignore the try block and continue.
  Out v (FTry _ _ : k)                  -> return $ Out v k

  -- If an exception rises all the way to the top level without being
  -- handled, turn it into an error message via the 'say' command.
  Up  exn []                            -> return $ In (TApp (TConst Say) (TString (formatExn exn))) V.empty [FExec]

  -- Fatal errors can't be caught; just throw away the continuation
  -- stack.
  Up exn@Fatal{} _                      -> return $ Up exn []

  -- Otherwise, if we are raising an exception up the continuation
  -- stack and come to a Try frame, execute the associated catch
  -- block.
  Up _ (FTry t e : k)                   -> return $ In t e (FExec : k)

  -- Otherwise, keep popping from the continuation stack.
  Up exn (_ : k)                        -> return $ Up exn k

  Out (VResult _ _) k -> do
    let msg = T.unlines
          [ "Bad machine state in stepRobot: no appropriate stack frame to catch a VResult"
          , from (show cek)
          ]
    return $ Up (Fatal msg) k

  Out _ (FExec : k) -> do
    let msg = T.unlines
          [ "Bad machine state in stepRobot: FExec frame with non-executable value"
          , from (show cek)
          ]
    return $ Up (Fatal msg) k

  -- Finally, if we're done evaluating and the continuation stack is
  -- empty, return the machine unchanged.
  done@(Out _ [])                        -> return done

-- | Determine whether a constant should take up a tick or not when executed.
takesTick :: Const -> Bool
takesTick c = isCmd c && (c `notElem` [Halt, Noop, Return, GetX, GetY, Ishere])

-- | At the level of the CEK machine there's no particular difference
--   between *evaluating* a function constant and *executing* a
--   command constant, but it somehow feels better to have two
--   different names for it anyway.
evalConst :: (MonadState GameState m, MonadIO m) => Const -> [Value] -> Cont -> StateT Robot m CEK
evalConst c vs k = do
  res <- runExceptT $ execConst c vs k
  case res of
    Left exn   -> do
      let msg = T.unlines
            [ "evalConst shouldn't be able to throw an exception:"
            , formatExn exn
            ]
      return $ Up (Fatal msg) k
    Right cek' -> return cek'


-- XXX load this from a file and have it available in a map?
--     Or make a quasiquoter?
seedProgram :: Text -> Term ::: TModule
seedProgram thing = prog
  where
    Right prog = processTerm . into @Text . unlines $
      [ "let repeat : int -> cmd () -> cmd () = \\n.\\c."
      , "  if (n == 0) {} {c ; repeat (n-1) c}"
      , "in {"
      , "  r <- random 500;"
      , "  repeat (r + 100) wait;"
      , "  appear \"|\";"
      , "  r <- random 500;"
      , "  repeat (r + 100) wait;"
      , "  place \"" ++ from @Text thing ++ "\";"
      , "  halt"
      , "}"
      ]

-- | Interpret the execution (or evaluation) of a constant application
--   to some values.
execConst :: (MonadState GameState m, MonadIO m) => Const -> [Value] -> Cont -> ExceptT Exn (StateT Robot m) CEK

execConst Noop _ k     = return $ Out VUnit k
execConst Return [v] k = return $ Out v k
execConst Return vs k  = badConst Return vs k
execConst Wait _ k     = return $ Out VUnit k
execConst Halt _ k     = do
  selfDestruct .= True
  flagRedraw
  return $ Out VUnit k

execConst Move _ k     = do
  loc <- use robotLocation
  orient <- use robotOrientation
  let nextLoc = loc ^+^ (orient ? zero)
  me <- entityAt nextLoc

  -- Make sure the robot has treads installed.
  E.treads `isInstalledOr` cmdExn Move ["You need treads to move."]

  -- Make sure nothing is in the way.
  maybe True (not . (`hasProperty` Unwalkable)) me `holdsOr`
    cmdExn Move ["There is a", fromJust me ^. entityName, "in the way!"]

  robotLocation .= nextLoc
  flagRedraw
  return $ Out VUnit k

execConst Grab _ k = do

  E.grabber `isInstalledOr` cmdExn Grab ["You need a grabber device to grab things."]

  -- Ensure there is an entity here.
  loc <- use robotLocation
  e <- entityAt loc >>= (`isJustOr` cmdExn Grab ["There is nothing here to grab."])

  -- Ensure it can be picked up.
  (e `hasProperty` Portable) `holdsOr`
    cmdExn Grab ["The", e ^. entityName, "here can't be grabbed."]

  -- Remove the entity from the world.
  updateEntityAt loc (const Nothing)
  flagRedraw

  when (e `hasProperty` Growable) $ do

    -- Grow a new entity from a seed.
    let seedBot =
          mkRobot "seed" loc (V2 0 0)
            (initMachine (seedProgram (e ^. entityName)) V.empty)
            []
            & robotDisplay .~
              (defaultEntityDisplay '.' & displayAttr .~ plantAttr)
            & robotInventory .~ E.singleton e
    _ <- lift . lift $ addRobot seedBot
    return ()

  -- Add the picked up item to the robot's inventory
  robotInventory %= insert e
  return $ Out VUnit k

execConst Turn [VDir d] k = do
  E.treads `isInstalledOr` cmdExn Turn ["You need treads to turn."]

  robotOrientation . _Just %= applyTurn d
  flagRedraw
  return $ Out VUnit k

execConst Turn args k = badConst Turn args k

-- XXX do we need a device to do placement?
execConst Place [VString s] k = do
  inv <- use robotInventory
  loc <- use robotLocation

  -- Make sure there's nothing already here
  nothingHere <- isNothing <$> entityAt loc
  nothingHere `holdsOr` cmdExn Place ["There is already an entity here."]

  -- Make sure the robot has the thing in its inventory
  e <- listToMaybe (lookupByName s inv) `isJustOr`
    cmdExn Place ["You don't have", indefinite s, "to place."]

  -- Place the entity and remove it from the inventory
  updateEntityAt loc (const (Just e))
  robotInventory %= delete e

  flagRedraw
  return $ Out VUnit k

execConst Place args k = badConst Place args k

-- XXX need a device to give --- but it should be available from the beginning
execConst Give [VString otherName, VString itemName] k = do

  -- Make sure the other robot is here
  _ <- robotNamed otherName >>=
    (`isJustOr` cmdExn Give ["There is no robot named", otherName, "here." ])

  -- Make sure we have the required item
  inv <- use robotInventory
  item <- (listToMaybe . lookupByName itemName $ inv) `isJustOr`
    cmdExn Give ["You don't have", indefinite itemName, "to give." ]

  -- Make the exchange
  lift . lift $ robotMap . at otherName . _Just . robotInventory %= insert item
  robotInventory %= delete item
  return $ Out VUnit k

execConst Give args k = badConst Give args k

-- XXX do we need a device to craft?
execConst Craft [VString name] k = do
  inv <- use robotInventory
  e <- listToMaybe (lookupByName name E.entityCatalog) `isJustOr`
    cmdExn Craft ["I've never heard of", indefiniteQ name, "."]

  recipe <- recipeFor e `isJustOr`
    cmdExn Craft ["There is no known recipe for crafting", indefinite name, "."]

  inv' <-  craft recipe inv `isRightOr` \missing ->
    cmdExn Craft ["Missing ingredients:", prettyIngredientList missing]

  robotInventory .= inv'
  return $ Out VUnit k

execConst Craft args k = badConst Craft args k


execConst GetX _ k = do
  V2 x _ <- use robotLocation
  return $ Out (VInt (fromIntegral x)) k
execConst GetY _ k = do
  V2 _ y <- use robotLocation
  return $ Out (VInt (fromIntegral y)) k

execConst Random [VInt hi] k = do
  n <- randomRIO (0, hi-1)
  return $ Out (VInt n) k
execConst Random args k = badConst Random args k

execConst Say [VString s] k = do
  rn <- use robotName
  lift . lift $ emitMessage (T.concat [rn, ": ", s])
  return $ Out VUnit k
execConst Say args k = badConst Say args k

execConst View [VString s] k = do
  _ <- robotNamed s >>=
    (`isJustOr` cmdExn View [ "There is no robot named ", s, " to view." ])

  lift . lift $ viewCenterRule .= VCRobot s
  return $ Out VUnit k
execConst View args k = badConst View args k

execConst Appear [VString s] k = do
  flagRedraw
  case into @String s of
    [c] -> do
      robotDisplay . defaultChar .= c
      robotDisplay . orientationMap .= M.empty
      return $ Out VUnit k

    [c,nc,ec,sc,wc] -> do
      robotDisplay . defaultChar .= c
      robotDisplay . orientationMap . ix north .= nc
      robotDisplay . orientationMap . ix east  .= ec
      robotDisplay . orientationMap . ix south .= sc
      robotDisplay . orientationMap . ix west  .= wc
      return $ Out VUnit k

    _other -> raise Appear [quote s, "is not a valid appearance string."]

execConst Appear args k = badConst Appear args k

execConst Ishere [VString s] k = do
  loc <-use robotLocation
  me <- entityAt loc
  case me of
    Nothing -> return $ Out (VBool False) k
    Just e  -> return $ Out (VBool (T.toLower (e ^. entityName) == T.toLower s)) k
execConst Ishere args k = badConst Ishere args k

execConst Not [VBool b] k = return $ Out (VBool (not b)) k
execConst Not args k = badConst Not args k

execConst (Cmp c) [v1, v2] k =
  case evalCmp c v1 v2 of
    Nothing -> return $ Out (VBool False) k  --- XXX random result?
    Just b  -> return $ Out (VBool b) k
execConst (Cmp c) args k     = badConst (Cmp c) args k

execConst (Arith Neg) [VInt n] k = return $ Out (VInt (-n)) k
execConst (Arith c) [VInt n1, VInt n2] k = return $ Out (VInt (evalArith c n1 n2)) k
execConst (Arith c) args k = badConst (Arith c) args k

execConst Force [VDelay t e] k = return $ In t e k
execConst Force args k = badConst Force args k

  -- Note, if should evaluate the branches lazily, but since
  -- evaluation is eager, by the time we get here thn and els have
  -- already been fully evaluated --- what gives?  The answer is that
  -- we rely on elaboration to add 'lazy' wrappers around the branches
  -- (and a 'force' wrapper around the entire if).
execConst If [VBool True , thn, _] k = return $ Out thn k
execConst If [VBool False, _, els] k = return $ Out els k
execConst If args k = badConst If args k

execConst Fst [VPair v _] k = return $ Out v k
execConst Fst args k        = badConst Fst args k
execConst Snd [VPair _ v] k = return $ Out v k
execConst Snd args k        = badConst Snd args k

execConst Try [VDelay t1 e1, VDelay t2 e2] k = return $ In t1 e1 (FExec : FTry t2 e2 : k)
execConst Try args k        = badConst Try args k

execConst Raise [VString s] k = return $ Up (User s) k
execConst Raise args k        = badConst Raise args k

execConst Build [VString name, VDelay c e] k = do
  r <- get
  let newRobot =
        mkRobot
          name
          (r ^. robotLocation)
          (r ^. robotOrientation ? east)
          (In c e [FExec])  -- XXX require cap for env that gets shared here?
          [E.treads, E.grabber, E.solarPanels]

  newRobot' <- lift . lift $ addRobot newRobot
  flagRedraw
  return $ Out (VString (newRobot' ^. robotName)) k
execConst Build args k = badConst Build args k

execConst Run [VString fileName] k = do
  mf <- liftIO $ readFileMay (into fileName)

  f <- mf `isJustOr` cmdExn Run ["File not found:", fileName]

  t <- processTerm (into @Text f)`isRightOr` \err ->
    cmdExn Run ["Error in", fileName, "\n", err]

  return $ initMachine' t V.empty k

execConst Run args k = badConst Run args k

badConst :: Monad m => Const -> [Value] -> Cont -> ExceptT Exn (StateT Robot m) a
badConst c args k = throwError $ Fatal $
  T.unwords
  [ "Bad application of execConst"
  , from (show c)
  , from (show args)
  , from (show k)
  ]

-- | Evaluate the application of a comparison operator.  Returns
--   @Nothing@ if the application does not make sense.
evalCmp :: CmpConst -> Value -> Value -> Maybe Bool
evalCmp c v1 v2 = decideCmp c <$> compareValues v1 v2

-- | Decide the result of a comparison operator, given the 'Ordering'
--   resulting from comparing two values.
decideCmp :: CmpConst -> Ordering -> Bool
decideCmp = \case
  CmpEq  -> (== EQ)
  CmpNeq -> (/= EQ)
  CmpLt  -> (== LT)
  CmpGt  -> (== GT)
  CmpLeq -> (/= GT)
  CmpGeq -> (/= LT)

-- | Compare two values, returning an 'Ordering' if they can be
--   compared, or @Nothing@ if they cannot.
compareValues :: Value -> Value -> Maybe Ordering
compareValues = \case
  VUnit         -> \case {VUnit      -> Just EQ             ; _ -> Nothing}
  VInt n1       -> \case {VInt n2    -> Just (compare n1 n2); _ -> Nothing}
  VString t1    -> \case {VString t2 -> Just (compare t1 t2); _ -> Nothing}
  VDir d1       -> \case {VDir d2    -> Just (compare d1 d2); _ -> Nothing}
  VBool b1      -> \case {VBool b2   -> Just (compare b1 b2); _ -> Nothing}
  VPair v11 v12 -> \case { VPair v21 v22
                           -> (<>) <$> compareValues v11 v21 <*> compareValues v12 v22
                         ; _ -> Nothing
                         }
  VClo{}        -> const Nothing
  VCApp{}       -> const Nothing
  VDef{}        -> const Nothing
  VResult{}     -> const Nothing
  VBind{}       -> const Nothing
  VDelay{}      -> const Nothing

-- XXX Split Arith into binary + unary so we don't have to have silly
-- special cases like this

-- | Evaluate the application of an arithmetic operator.
evalArith :: ArithConst -> Integer -> Integer -> Integer
evalArith Neg = error "evalArith Neg: should have been handled already"
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = safeDiv
evalArith Exp = (^)

safeDiv :: Integer -> Integer -> Integer
safeDiv _ 0 = 42
safeDiv a b = a `div` b
