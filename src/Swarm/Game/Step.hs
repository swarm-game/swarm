-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Step
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Facilities for stepping the robot CEK machines, *i.e.* the actual
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
import           Control.Monad.State
import qualified Data.Map                as M
import           Data.Maybe              (listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Linear
import           System.Random           (randomRIO)
import           Witch

import           Swarm.Game.CEK
import           Swarm.Game.Display
import           Swarm.Game.Entities     as E
import           Swarm.Game.Entity       as E
import           Swarm.Game.Recipes
import           Swarm.Game.Robot
import           Swarm.Game.State
import           Swarm.Game.Value        as V
import qualified Swarm.Game.World        as W
import           Swarm.Language.Pipeline
import           Swarm.Language.Pretty
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
        robotMap %= M.update (const curRobot') rn

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

-- | Run a robot for one "big step", which may consist of up to
--   'evalStepsPerTick' CEK machine steps and at most one command
--   execution.
bigStepRobot :: (MonadState GameState m, MonadIO m) => Robot -> m (Maybe Robot)
bigStepRobot = bigStepRobotRec . (tickSteps .~ evalStepsPerTick)

-- | Recursive helper function for 'bigStepRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
bigStepRobotRec :: (MonadState GameState m, MonadIO m) => Robot -> m (Maybe Robot)
bigStepRobotRec r
  | not (isActive r) || r ^. tickSteps <= 0 = return (Just r)
  | otherwise           = do
      r' <- stepRobot r
      maybe (return Nothing) bigStepRobotRec r'

-- | Helper function for accomplishing a single step: given a robot
--   and a new CEK machine state, decrement its @tickSteps@ and set
--   its CEK machine to the new state.  This function always returns
--   @Just@ a robot.
step :: (MonadState GameState m, MonadIO m) => Robot -> CEK -> m (Maybe Robot)
step r cek = do

  -- for debugging. Uncomment to get a sequence of CEK machine states
  -- printed to an output file.
  -- liftIO $ appendFile "out.txt" (prettyCEK (r ^. machine))

  return . Just $ r & tickSteps -~ 1 & machine .~ cek

-- | Step to an updated robot and continuation, producing a unit-value
--   output.  @stepUnit r k == step r (Out VUnit k)@.
stepUnit :: (MonadState GameState m, MonadIO m) => Robot -> Cont -> m (Maybe Robot)
stepUnit r k = step r $ Out VUnit k

-- | The main CEK machine workhorse.  Given a robot, look at its CEK
--   machine state and figure out a single next step. The reason we
--   return a @Maybe Robot@ is that the robot could execute a 'Halt'
--   instruction, making it disappear, which we signal by returning
--   @Nothing@.
stepRobot :: (MonadState GameState m, MonadIO m) => Robot -> m (Maybe Robot)
stepRobot r = case r ^. machine of

  ------------------------------------------------------------
  -- Evaluation

  -- First some straightforward cases.  These all immediately turn
  -- into values.
  In TUnit _ k                      -> stepUnit r k
  In (TDir d) _ k                   -> step r $ Out (VDir d) k
  In (TInt n) _ k                   -> step r $ Out (VInt n) k
  In (TString s) _ k                -> step r $ Out (VString s) k
  In (TBool b) _ k                  -> step r $ Out (VBool b) k

  -- A constant is turned into a VCApp which might be waiting for arguments.
  In (TConst c) _ k                 -> step r $ Out (VCApp c []) k

  -- To evaluate a variable, just look it up in the context.
  In (TVar x) e k                   -> step r $ Out (e !!! x) k

  -- To evaluate a pair, start evaluating the first component.
  In (TPair t1 t2) e k              -> step r $ In t1 e (FSnd t2 e : k)
  -- Once that's done, evaluate the second component.
  Out v1 (FSnd t2 e : k)            -> step r $ In t2 e (FFst v1 : k)
  -- Finally, put the results together into a pair value.
  Out v2 (FFst v1 : k)              -> step r $ Out (VPair v1 v2) k

  -- Lambdas immediately turn into closures.
  In (TLam x _ t) e k               -> step r $ Out (VClo x t e) k

  -- To evaluate an application, start by focusing on the left-hand
  -- side and saving the argument for later.
  In (TApp t1 t2) e k               -> step r $ In t1 e (FArg t2 e : k)
  -- Once that's done, switch to evaluating the argument.
  Out v1 (FArg t2 e : k)            -> step r $ In t2 e (FApp v1 : k)
  -- We can evaluate an application of a closure in the usual way.
  Out v2 (FApp (VClo x t e) : k)    -> step r $ In t (addBinding x v2 e) k
  -- We can also evaluate an application of a constant by collecting
  -- arguments, eventually dispatching to evalConst for function
  -- constants.
  Out v2 (FApp (VCApp c args) : k)
    | not (isCmd c) &&
      arity c == length args + 1    -> evalConst c (reverse (v2 : args)) k r
    | otherwise                     -> step r $ Out (VCApp c (v2 : args)) k
  cek@(Out _ (FApp _ : _))         ->
    error $ "Panic! Bad machine state in stepRobot, FApp of non-function: " ++ show cek

  -- Evaluating let expressions is a little bit tricky. We start by
  -- focusing on the let-bound expression. But since it is allowed to
  -- be recursive, we have to set up a recursive environment in which
  -- to evaluate it.  Note how we wrap the expression in VDelay; the
  -- elaboration step wrapped all recursive references in a
  -- corresponding @Force@.
  In (TLet x _ t1 t2) e k           ->
    let e' = addBinding x (VDelay t1 e') e   -- XXX do this without making a recursive
                                             -- (hence unprintable) env?
    in step r $ In t1 e' (FLet x t2 e : k)

  -- Once we've finished with the let-binding, we switch to evaluating
  -- the body in a suitably extended environment.
  Out v1 (FLet x t2 e : k)          -> step r $ In t2 (addBinding x v1 e) k

  -- Definitions immediately turn into VDef values, awaiting execution.
  In (TDef x _ t) e k               -> step r $ Out (VDef x t e) k

  -- To evaluate a bind expression, we evaluate the first command.
  In (TBind mx t1 t2) e k           -> step r $ In t1 e (FEvalBind mx t2 e : k)
  -- Once we're done evaluating (NOT executing!) the first command to a value,
  -- we put it back in a VBind value to await execution.
  Out v1 (FEvalBind mx t2 e : k)    -> step r $ Out (VBind v1 mx t2 e) k

  -- Delay expressions immediately turn into VDelay values, awaiting
  -- application of 'Force'.
  In (TDelay t) e k                  -> step r $ Out (VDelay t e) k

  ------------------------------------------------------------
  -- Execution

  -- To execute a definition, we focus on evaluating the body in a
  -- recursive environment (similar to let), and remember that the
  -- result should be bound to the given name.
  Out (VDef x t e) (FExec : k)       ->
    let e' = addBinding x (VDelay t e') e
    in  step r $ In t e' (FDef x : k)
  -- Once we are done evaluating the body of a definition, we return a
  -- special VResult value, which packages up the return value from
  -- the @def@ command itself (@unit@) together with the resulting
  -- environment (the variable bound to the value).
  Out v  (FDef x : k)                -> step r $ Out (VResult VUnit (V.singleton x v)) k

  -- To execute a constant application, delegate to the 'execConst'
  -- function.  Set tickSteps to 0 if the command is supposed to take
  -- a tick, so the robot won't take any more steps this tick.
  Out (VCApp c args) (FExec : k)     ->
    execConst c (reverse args) k (r & if takesTick c then tickSteps .~ 0 else id)

  -- To execute a bind expression, execute the first command, and
  -- remember the second for execution later.
  Out (VBind c mx t2 e) (FExec : k)  -> step r $ Out c (FExec : FExecBind mx t2 e : k)
  -- If first command completes with a value along with an environment
  -- resulting from definition commands, switch to evaluating the
  -- second command of the bind.  Extend the environment with both the
  -- definition environment resulting from the first command, as well
  -- as a binding for the result (if the bind was of the form @x <-
  -- c1; c2@).  Remember that we must execute the second command once
  -- it has been evaluated, then union any resulting definition
  -- environment with the definition environment from the first
  -- command.
  Out (VResult v ve) (FExecBind mx t2 e : k) ->
    step r $ In t2 (ve `V.union` maybe id (`addBinding` v) mx e) (FExec : FUnionEnv ve : k)
  -- On the other hand, if the first command completes with a simple value,
  -- we do something similar, but don't have to worry about the environment.
  Out v (FExecBind mx t2 e : k) ->
    step r $ In t2 (maybe id (`addBinding` v) mx e) (FExec : k)
  -- If a command completes with a value and definition environment,
  -- and the next continuation frame contains a previous environment
  -- to union with, then pass the unioned environments along in
  -- another VResult.
  Out (VResult v e2) (FUnionEnv e1 : k) -> step r $ Out (VResult v (e2 `V.union` e1)) k
  -- Or, if a command completes with no environment, but there is a
  -- previous environment to union with, just use that environment.
  Out v (FUnionEnv e : k)               -> step r $ Out (VResult v e) k

  -- If the top of the continuation stack contains a 'FLoadEnv' frame,
  -- it means we are supposed to load up the resulting definition
  -- environment and type context into the robot's top-level
  -- environment and type context, so they will be available to future
  -- programs.
  Out (VResult v e) (FLoadEnv ctx : k) ->
    step (r & robotEnv %~ V.union e & robotCtx %~ M.union ctx) $ Out v k
  Out v (FLoadEnv _ : k)             -> step r $ Out v k

  cek@(Out (VResult _ _) _) ->
    error $ "Panic! Bad machine state in stepRobot: no appropriate stack frame to catch a VResult: " ++ show cek

  cek@(Out _ (FExec : _)) ->
    error $ "Panic! Bad machine state in stepRobot: FExec frame with non-executable value: " ++ show cek

  -- Finally, if there's nothing left to do, just return without
  -- taking a step at all.
  Out _ []                          -> return (Just r)

-- | Determine whether a constant should take up a tick or not when executed.
takesTick :: Const -> Bool
takesTick c = isCmd c && (c `notElem` [Halt, Noop, Return, GetX, GetY, Ishere])

-- | Emit a formatted error message.
emitError :: MonadState GameState m => Robot -> Const -> [Text] -> m ()
emitError r c parts = emitMessage (formatError r c parts)

-- | Create a standard formatted error containing the robot name and
--   the name of the command that caused the error.
formatError :: Robot -> Const -> [Text] -> Text
formatError r c = T.unwords . (colon (r ^. robotName) :) . (colon (prettyText c) :)
  where
    colon = flip T.append ":"

-- | Require that a robot has a given device installed, OR we are in
--   creative mode.  Run the first (failure) continuation if not, and
--   the second (success) continuation if so.
require :: MonadState GameState m => Robot -> Entity -> m a -> m a -> m a
require r e fk sk = do
  mode <- use gameMode
  if mode == Creative || r `hasInstalled` e then sk else fk

-- | At the level of the CEK machine there's no particular difference
--   between *evaluating* a function constant and *executing* a
--   command constant, but it somehow feels better to have two
--   different names for it anyway.
evalConst :: (MonadState GameState m, MonadIO m) => Const -> [Value] -> Cont -> Robot -> m (Maybe Robot)
evalConst = execConst

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

-- XXX rewrite nested pattern-matching code nicely using ideas from
-- https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html

-- | Interpret the execution (or evaluation) of a constant application
--   to some values.
execConst :: (MonadState GameState m, MonadIO m) => Const -> [Value] -> Cont -> Robot -> m (Maybe Robot)

execConst Noop _ k r     = stepUnit r k
execConst Return [v] k r = step r $ Out v k
execConst Return vs k _  = badConst Return vs k
execConst Wait _ k r     = stepUnit r k
execConst Halt _ _ _     = updated .= True >> return Nothing

execConst Move _ k r     = do
  let V2 x y = (r ^. robotLocation) ^+^ (r ^. robotOrientation ? zero)
  me <- uses world (W.lookupEntity (-y,x))
  require r E.treads
    (emitError r Move ["You need treads to move."] >> stepUnit r k)
    case maybe True (not . (`hasProperty` Unwalkable)) me of

      -- There's something there, and we can't walk on it.
      -- It seems like it would be super annoying for this to generate
      -- an error message or throw an exception!  We just consider this
      -- normal operation of the Move instruction when a robot is blocked.
      False -> stepUnit r k

      -- Otherwise, move forward.
      True -> do
        updated .= True
        stepUnit (r & robotLocation %~ (^+^ (r ^. robotOrientation ? zero))) k

execConst Grab _ k r =
  require r E.grabber
    (emitError r Grab ["You need a grabber device to grab things."] >> stepUnit r k)
    do
      let V2 x y = r ^. robotLocation
      me <- uses world (W.lookupEntity (-y,x))
      case me of

        -- No entity here.
        Nothing -> emitError r Grab ["There is nothing here to grab."] >> stepUnit r k

        -- There is an entity here...
        Just e -> case e `hasProperty` Portable of

          -- ...but it can't be picked up.
          False -> do
            emitError r Grab ["The", e ^. entityName, "here can't be grabbed."]
            stepUnit r k

          -- ...and it can be picked up.
          True -> do
            updated .= True

            -- Remove the entity from the world.
            world %= W.update (-y,x) (const Nothing)

            when (e `hasProperty` Growable) $ do

              -- Grow a new entity from a seed.
              let seedBot =
                    mkRobot "seed" (r ^. robotLocation) (V2 0 0)
                      (initMachine (seedProgram (e ^. entityName)) V.empty)
                      []
                      & robotDisplay .~
                        (defaultEntityDisplay '.' & displayAttr .~ plantAttr)
                      & robotInventory .~ E.singleton e
              _ <- addRobot seedBot
              return ()

            -- Add the picked up item to the robot's inventory
            stepUnit (r & robotInventory %~ insert e) k

execConst Turn [VDir d] k r = do
  require r E.treads
    (emitError r Turn ["You need treads to turn."] >> stepUnit r k)
    do
      updated .= True
      stepUnit (r & robotOrientation . _Just %~ applyTurn d) k
execConst Turn args k _ = badConst Turn args k

-- XXX do we need a device to do placement?
execConst Place [VString s] k r = do
  let V2 x y = r ^. robotLocation
  me <- uses world (W.lookupEntity (-y, x))
  case me of
    Just _  -> emitError r Place ["There is already an entity here."] >> stepUnit r k
    Nothing -> case lookupByName s (r ^. robotInventory) of
      []    -> emitError r Place ["You don't have", indefinite s, "to place."] >> stepUnit r k
      (e:_) -> do
        updated .= True
        world %= W.update (-y, x) (const (Just e))
        stepUnit (r & robotInventory %~ delete e) k
execConst Place args k _ = badConst Place args k

-- XXX need a device to give --- but it should be available from the beginning
execConst Give [VString otherName, VString itemName] k r = do
  mother <- use (robotMap . at otherName)
  let mitem = listToMaybe . lookupByName itemName $ (r ^. robotInventory)
  case (mother, mitem) of
    (Just _, Just item) -> do
      robotMap . at otherName . _Just . robotInventory %= insert item
      stepUnit (r & robotInventory %~ delete item) k
    (Nothing, _) -> do
      emitError r Give ["There is no robot named", otherName, "here." ]
      stepUnit r k
    (_, Nothing) -> do
      emitError r Give ["You don't have", indefinite itemName, "to give." ]
      stepUnit r k

execConst Give args k _ = badConst Give args k

-- XXX do we need a device to craft?
execConst Craft [VString name] k r =
  case listToMaybe $ lookupByName name E.entityCatalog of
    Nothing -> emitError r Craft ["I've never heard of", indefiniteQ name, "."] >> stepUnit r k
    Just e  -> case recipeFor e of
      Nothing -> do
        emitError r Craft ["There is no known recipe for crafting", indefinite name, "."]
        stepUnit r k
      Just recipe -> case craft recipe (r ^. robotInventory) of
        -- XXX describe the missing ingredients
        Left missing -> do
          emitError r Craft ["Missing ingredients:", prettyIngredientList missing]
          stepUnit r k
        Right inv'    -> stepUnit (r & robotInventory .~ inv') k

execConst Craft args k _ = badConst Craft args k


execConst GetX _ k r = step r $ Out (VInt (fromIntegral x)) k
  where
    V2 x _ = r ^. robotLocation
execConst GetY _ k r = step r $ Out (VInt (fromIntegral y)) k
  where
    V2 _ y = r ^. robotLocation

execConst Random [VInt hi] k r = do
  n <- randomRIO (0, hi-1)
  step r $ Out (VInt n) k
execConst Random args k _ = badConst Random args k

execConst Say [VString s] k r = do
  emitMessage (T.concat [r ^. robotName, ": ", s])
  stepUnit r k
execConst Say args k _ = badConst Say args k

execConst View [VString s] k r = do
  mr <- use (robotMap . at s)
  case mr of
    Nothing -> emitError r View [ "There is no robot named ", s, " to view." ]
    Just _  -> viewCenterRule .= VCRobot s
  stepUnit r k
execConst View args k _ = badConst View args k

execConst Appear [VString s] k r = do
  updated .= True
  case into @String s of
    [c] ->
      stepUnit
        (r & robotDisplay . defaultChar .~ c
           & robotDisplay . orientationMap .~ M.empty
        )
        k
    [c,nc,ec,sc,wc] ->
      stepUnit
        ( r & robotDisplay . defaultChar .~ c
            & robotDisplay . orientationMap . ix north .~ nc
            & robotDisplay . orientationMap . ix east  .~ ec
            & robotDisplay . orientationMap . ix south .~ sc
            & robotDisplay . orientationMap . ix west  .~ wc
        )
        k
    _other -> do
      emitError r Appear [quote s, "is not a valid appearance string."]
      stepUnit r k
execConst Appear args k _ = badConst Appear args k

execConst Ishere [VString s] k r = do
  let V2 x y = r ^. robotLocation
  me <- uses world (W.lookupEntity (-y, x))
  -- XXX encapsulate the above into a function, used several times

  case me of
    Nothing -> step r $ Out (VBool False) k
    Just e  -> step r $ Out (VBool (T.toLower (e ^. entityName) == T.toLower s)) k
execConst Ishere args k _ = badConst Ishere args k

execConst Not [VBool b] k r = step r $ Out (VBool (not b)) k
execConst Not args k _ = badConst Not args k

execConst (Cmp c) [v1, v2] k r =
  case evalCmp c v1 v2 of
    Nothing -> step r $ Out (VBool False) k  --- XXX raise an exception!
    Just b  -> step r $ Out (VBool b) k
execConst (Cmp c) args k _     = badConst (Cmp c) args k

execConst (Arith Neg) [VInt n] k r = step r $ Out (VInt (-n)) k
execConst (Arith c) [VInt n1, VInt n2] k r = step r $ Out (VInt (evalArith c n1 n2)) k
execConst (Arith c) args k _ = badConst (Arith c) args k

execConst Force [VDelay t e] k r = step r $ In t e k
execConst Force args k _ = badConst Force args k

  -- Note, if should evaluate the branches lazily, but since
  -- evaluation is eager, by the time we get here thn and els have
  -- already been fully evaluated --- what gives?  The answer is that
  -- we rely on elaboration to add 'lazy' wrappers around the branches
  -- (and a 'force' wrapper around the entire if).
execConst If [VBool True , thn, _] k r = step r $ Out thn k
execConst If [VBool False, _, els] k r = step r $ Out els k
execConst If args k _ = badConst If args k

execConst Fst [VPair v _] k r = step r $ Out v k
execConst Fst args k _        = badConst Fst args k
execConst Snd [VPair _ v] k r = step r $ Out v k
execConst Snd args k _        = badConst Snd args k

execConst Build [VString name, VDelay c e] k r = do
  let newRobot =
        mkRobot
          name
          (r ^. robotLocation)
          (r ^. robotOrientation ? east)
          (In c e [FExec])  -- XXX require cap for env that gets shared here?
          [E.treads, E.grabber, E.solarPanels]

  newRobot' <- addRobot newRobot
  updated .= True
  step r $ Out (VString (newRobot' ^. robotName)) k
execConst Build args k _ = badConst Build args k

execConst Run [VString fileName] k r = do
  mf <- liftIO $ readFileMay (into fileName)
  case mf of
    Nothing -> emitError r Run ["File not found:", fileName] >> stepUnit r k
    Just f -> case processTerm (into @Text f) of
      Left  err        -> do
        emitError r Run ["Error while processing", fileName, "\n", err]
        stepUnit r k
      Right t -> step r $ initMachine' t V.empty k

      -- Note, adding FExec to the stack above in the TyCmd case (done
      -- automatically by the initMachine function) is correct.  run
      -- has the type run : String -> Cmd (), i.e. executing (run s)
      -- for some string s causes it to load *and immediately execute*
      -- the program in the file.
      --
      -- If we instead had
      --
      --   load : String -> Cmd (Cmd ())
      --
      -- (which could indeed be useful, once commands have return values
      -- and Bind does more than just sequencing) then the code would be
      -- the same as for run, EXCEPT that we would NOT add the FExec to
      -- the stack above.  The fact that there are two FExec frames
      -- involved in executing 'run' (one to execute the run command
      -- itself, and one to execute the thing it loads) corresponds to
      -- the fact that it is equivalent to (in pseudo-Haskell syntax)
      -- 'join . load'.

execConst Run args k _ = badConst Run args k

badConst :: Const -> [Value] -> Cont -> a
badConst c args k = error $
  "Panic! Bad application of execConst " ++ show c ++ " " ++ show args ++ " " ++ show k

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

-- | Evaluate the application of an arithmetic operator.
evalArith :: ArithConst -> Integer -> Integer -> Integer
evalArith Neg = error "evalArith Neg: should have been handled already"
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = div
evalArith Exp = (^)
