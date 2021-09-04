-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The implementation of the Swarm game itself, as separate from the UI.
-- XXX
--
-----------------------------------------------------------------------------

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.Game
  ( -- * Values

    Value(..), prettyValue

    -- * Constructing CEK machine states

  , initMachine, initMachineV, idleMachine

    -- * Running the game

  , gameStep

    -- * Displays

  , Display, lookupDisplay, displayAttr, displayPriority

    -- * Robots

  , Robot, isActive
  , robotEntity
  , robotName, robotDisplay, robotLocation, robotOrientation, robotInventory
  , machine, tickSteps

    -- * Game state
  , ViewCenterRule(..), updateViewCenter, manualViewCenterUpdate
  , GameState(..), initGameState, viewCenterRule

    -- ** Lenses

  , gameMode, robotMap, newRobots, world, viewCenter, updated, replResult
  , messageQueue

    -- * Convenience re-exports

  , module Swarm.Game.Entity
  )
  where

import           Control.Arrow           ((&&&))
import           Control.Lens            hiding (Const, contains, from, parts)
import           Control.Monad.State
import           Data.Bifunctor          (first)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Linear
import           System.Random           (randomRIO)
import           Witch

-- import           Data.Hash.Murmur

import           Swarm.Game.CEK
import           Swarm.Game.Display
import qualified Swarm.Game.Entities     as E
import           Swarm.Game.Entity
import           Swarm.Game.Recipes
import           Swarm.Game.Robot
import           Swarm.Game.Value
import qualified Swarm.Game.World        as W
import           Swarm.Game.WorldGen     (findGoodOrigin, testWorld2)
import           Swarm.Language.Pipeline
import           Swarm.Language.Pretty
import           Swarm.Language.Syntax
import           Swarm.Language.Types
import           Swarm.TUI.Attr
import           Swarm.Util

------------------------------------------------------------
-- Game state data types
------------------------------------------------------------

data ViewCenterRule
  = VCLocation (V2 Int)
  | VCRobot Text
  deriving (Eq, Ord, Show)

data GameMode
  = Classic
  | Creative
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data GameState = GameState
  { _gameMode       :: GameMode
  , _robotMap       :: Map Text Robot
  , _newRobots      :: [Robot]
  , _gensym         :: Int
  , _world          :: W.TileCachingWorld Int Entity
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter     :: V2 Int
  , _updated        :: Bool
  , _replResult     :: Maybe (Type, Maybe Value)
  , _messageQueue   :: [Text]
  }

makeLenses ''GameState

applyViewCenterRule :: ViewCenterRule -> Map Text Robot -> Maybe (V2 Int)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . robotLocation

updateViewCenter :: GameState -> GameState
updateViewCenter g = g
  & viewCenter .~ newViewCenter
  & (if newViewCenter /= oldViewCenter then updated .~ True else id)
  where
    oldViewCenter = g ^. viewCenter
    newViewCenter = fromMaybe oldViewCenter (applyViewCenterRule (g ^. viewCenterRule) (g ^. robotMap))

manualViewCenterUpdate :: (V2 Int -> V2 Int) -> GameState -> GameState
manualViewCenterUpdate update g = g
  & case g ^. viewCenterRule of
      VCLocation l -> viewCenterRule .~ VCLocation (update l)
      VCRobot _    -> viewCenterRule .~ VCLocation (update (g ^. viewCenter))
  & updateViewCenter

ensureUniqueName :: MonadState GameState m => Robot -> m Robot
ensureUniqueName newRobot = do
  -- See if another robot already has the same name...
  let name = newRobot ^. robotName
  collision <- uses robotMap (M.member name)
  case collision of
    -- If so, add a suffix to make the name unique.
    True -> do
      tag <- gensym <+= 1
      let name' = name `T.append` into @Text (show tag)
      return $ newRobot & robotName .~ name'
    False -> return newRobot

addRobot :: MonadState GameState m => Robot -> m Robot
addRobot r = do
  r' <- ensureUniqueName r
  newRobots %= (r' :)
  return r'

initGameState :: IO GameState
initGameState = return $
  GameState
  { _gameMode       = Classic
  , _robotMap       = M.singleton "base" baseRobot
  , _newRobots      = []
  , _gensym         = 0
  , _world          = W.newWorld . fmap (first fromEnum) . findGoodOrigin $ testWorld2
  , _viewCenterRule = VCRobot "base"
  , _viewCenter     = V2 0 0
  , _updated        = False
  , _replResult     = Nothing
  , _messageQueue   = []
  }

maxMessageQueueSize :: Int
maxMessageQueueSize = 1000

emitMessage :: MonadState GameState m => Text -> m ()
emitMessage msg = do
  q <- use messageQueue
  messageQueue %= (msg:) . (if length q >= maxMessageQueueSize then init else id)

------------------------------------------------------------
-- CEK machine

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
gameStep :: GameState -> IO GameState
gameStep = execStateT $ do

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
    Just r  -> unless (isActive r) $ replResult . _Just . _2 .= getResult r
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
bigStepRobot :: Robot -> StateT GameState IO (Maybe Robot)
bigStepRobot = bigStepRobotRec . (tickSteps .~ evalStepsPerTick)

-- | Recursive helper function for 'bigStepRobot', which checks if the
--   robot is actively running and still has steps left, and if so
--   runs it for one step, then calls itself recursively to continue
--   stepping the robot.
bigStepRobotRec :: Robot -> StateT GameState IO (Maybe Robot)
bigStepRobotRec r
  | not (isActive r) || r ^. tickSteps <= 0 = return (Just r)
  | otherwise           = do
      r' <- stepRobot r
      maybe (return Nothing) bigStepRobotRec r'

-- | Helper function for accomplishing a single step: given a robot
--   and a new CEK machine state, decrement its @tickSteps@ and set
--   its CEK machine to the new state.  This function always returns
--   @Just@ a robot.
step :: Robot -> CEK -> StateT GameState IO (Maybe Robot)
step r cek = do

  -- for debugging. Uncomment to get a sequence of CEK machine states
  -- printed to an output file.
  -- liftIO $ appendFile "out.txt" (prettyCEK (r ^. machine))

  return . Just $ r & tickSteps -~ 1 & machine .~ cek

-- | Step to an updated robot and continuation, producing a unit-value
--   output.  @stepUnit r k == step r (Out VUnit k)@.
stepUnit :: Robot -> Cont -> StateT GameState IO (Maybe Robot)
stepUnit r k = step r $ Out VUnit k

-- | The main CEK machine workhorse.  Given a robot, look at its CEK
--   machine state and figure out a single next step. The reason we
--   return a @Maybe Robot@ is that the robot could execute a 'Halt'
--   instruction, making it disappear, which we signal by returning
--   @Nothing@.
stepRobot :: Robot -> StateT GameState IO (Maybe Robot)
stepRobot r = case r ^. machine of

  -- First a bunch of straightforward cases.  These are all
  -- immediately turned into values.
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

  -- Lambdas just immediately turn into closures.
  In (TLam x _ t) e k               -> step r $ Out (VClo x t e) k

  -- To evaluate an application, focus on the left-hand side and save
  -- the right-hand side for later.
  In (TApp _ t1 t2) e k             -> step r $ In t1 e (FArg t2 e : k)

  -- Evaluating let expressions is a little bit tricky. XXX write more
  In (TLet x _ t1 t2) e k           ->
    let e' = addBinding x (VDelay t1 e') e   -- XXX do this without making a recursive
                                             -- (hence unprintable) env?
    in step r $ In t1 e' (FLet x t2 e : k)
  In (TBind mx _ t1 t2) e k         -> step r $ In t1 e (FEvalBind mx t2 e : k)
  In (TDelay t) e k                 -> step r $ Out (VDelay t e) k

  Out _ []                          -> return (Just r)

  Out v1 (FSnd t2 e : k)            -> step r $ In t2 e (FFst v1 : k)
  Out v2 (FFst v1 : k)              -> step r $ Out (VPair v1 v2) k
  Out v1 (FArg t2 e : k)            -> step r $ In t2 e (FApp v1 : k)
  Out v2 (FApp (VCApp c args) : k)
    | not (isCmd c) &&
      arity c == length args + 1    -> evalConst c (reverse (v2 : args)) k r
    | otherwise                     -> step r $ Out (VCApp c (v2 : args)) k
  Out v2 (FApp (VClo x t e) : k)    -> step r $ In t (addBinding x v2 e) k
  Out v1 (FLet x t2 e : k)          -> step r $ In t2 (addBinding x v1 e) k
  Out v1 (FEvalBind mx t2 e : k)    -> step r $ Out (VBind v1 mx t2 e) k

  -- Special command applications that don't use up a tick (Noop and Return)
  Out (VCApp Noop _) (FExec : k)     -> stepUnit r k
  Out (VCApp Return [v]) (FExec : k) -> step r $ Out v k

  Out (VCApp c args) (FExec : k)    -> execConst c (reverse args) k (r & tickSteps .~ 0)
  Out (VBind c mx t2 e) (FExec : k) -> step r $ Out c (FExec : FExecBind mx t2 e : k)
  Out v (FExecBind mx t2 e : k)     -> step r $ In t2 (maybe id (`addBinding` v) mx e) (FExec : k)

  cek -> error $ "Panic! Bad machine state in stepRobot: " ++ show cek

emitError :: MonadState GameState m => Robot -> Const -> [Text] -> m ()
emitError r c parts = emitMessage (formatError r c parts)

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
evalConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
evalConst = execConst

-- XXX load this from a file and have it available in a map?
--     Or make a quasiquoter?
seedProgram :: Text -> ATerm
seedProgram thing = prog
  where
    Right (prog ::: _) = processTerm . into @Text . unlines $
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

execConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
execConst Wait _ k r   = stepUnit r k
execConst Halt _ _ _   = updated .= True >> return Nothing
execConst Return _ _ _ = error "execConst Return should have been handled already in stepRobot!"
execConst Noop _ _ _   = error "execConst Noop should have been handled already in stepRobot!"
execConst Move _ k r   = do
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
                      (initMachine (seedProgram (e ^. entityName)) (TyCmd TyUnit))
                      []
                      & robotDisplay .~
                        (defaultEntityDisplay '.' & displayAttr .~ plantAttr)
                      & robotInventory .~ singleton e
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

execConst (Cmp c) [VInt n1, VInt n2] k r = step r $ Out (VBool (evalCmp c n1 n2)) k
execConst (Cmp c) args k _ = badConst (Cmp c) args k

execConst (Arith Neg) [VInt n] k r = step r $ Out (VInt (-n)) k
execConst (Arith c) [VInt n1, VInt n2] k r = step r $ Out (VInt (evalArith c n1 n2)) k
execConst (Arith c) args k _ = badConst (Arith c) args k

execConst Force [VDelay t e] k r = step r $ In t e k
execConst Force args k _ = badConst Force args k

  -- Note, if should evaluate the branches lazily, but since
  -- evaluation is eager, by the time we get here thn and els have
  -- already been fully evaluated --- what ves?  The answer is that
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
    Just f -> case processCmd (into @Text f) of
      Left  _ -> emitError r Run ["Error while processing", fileName] >> stepUnit r k
      Right t -> step r $ In (erase t) M.empty (FExec : k)
      -- Note, adding FExec to the stack above is correct.  run has the
      --   type run : String -> Cmd (), i.e. executing (run s) for some
      --   string s causes it to load *and immediately execute* the
      --   program in the file.
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

evalCmp :: CmpConst -> Integer -> Integer -> Bool
evalCmp CmpEq  = (==)
evalCmp CmpNeq = (/=)
evalCmp CmpLt  = (<)
evalCmp CmpGt  = (>)
evalCmp CmpLeq = (<=)
evalCmp CmpGeq = (>=)

evalArith :: ArithConst -> Integer -> Integer -> Integer
evalArith Neg = error "evalArith Neg: should have been handled already"
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = div
evalArith Exp = (^)
