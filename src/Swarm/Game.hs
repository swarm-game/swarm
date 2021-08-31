{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
  -- no-warn-unused-binds is for debugging code

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The implementation of the Swarm game itself, as separate from the UI.
--
-----------------------------------------------------------------------------

module Swarm.Game
  ( -- * The CEK abstract machine

    -- | The Swarm interpreter uses a technique known as a
    --   <https://matt.might.net/articles/cek-machines/ CEK machine>.
    --   Execution happens simply by iterating a step function,
    --   sending one state of the CEK machine to the next. In addition
    --   to being relatively efficient, this means we can easily run a
    --   bunch of robots synchronously, in parallel, without resorting
    --   to any threads (by stepping their machines in a round-robin
    --   fashion); pause and single-step the game; save and resume,
    --   and so on.
    --
    --   Essentially, a CEK machine state has three components:
    --
    --   - The __C__ontrol is the thing we are currently focused on:
    --     either a 'UTerm' to evaluate, or a 'Value' that we have
    --     just finished evaluating.
    --   - The __E__nvironment ('Env') is a mapping from variables that might
    --     occur free in the Control to their values.
    --   - The __K__ontinuation ('Cont') is a stack of 'Frame's,
    --     representing the evaluation context, /i.e./ what we are
    --     supposed to do after we finish with the currently focused
    --     thing.  When we reduce the current term to a value, the top
    --     frame on the stack tells us how to proceed.
    --
    --   You can think of a CEK machine as a defunctionalization of a
    --   recursive big-step interpreter, where we explicitly keep
    --   track of the call stack and the environments that would be in
    --   effect at various places in the recursion.
    --
    --   The slightly confusing thing about CEK machines is how we
    --   have to pass around environments everywhere.  Basically,
    --   anywhere there can be unevaluated terms containing free
    --   variables (in values, in continuation stack frames, ...), we
    --   have to store the proper environment alongside so that when
    --   we eventually get around to evaluating it, we will be able to
    --   pull out the environment to use.

    -- ** Values
    Value(..), prettyValue, Env, emptyEnv

    -- ** Frames

  , Frame(..), Cont

    -- ** CEK machine states

  , CEK(..), initMachine, initMachineV, idleMachine

    -- ** Stepping the machine

  , gameStep, step, evalStepsPerTick
  , bigStepRobot, stepRobot, execConst

    -- * Items

  , Item(..)

    -- * Robots

  , RobotDisplay, defaultChar, dirMap, robotDisplayAttr, priority
  , lookupRobotDisplay, defaultRobotDisplay
  , Robot(..), mkRobot, baseRobot, isActive

    -- ** Lenses
  , robotName, robotDisplay, location, direction, machine, tickSteps, static

    -- * Game state
  , ViewCenterRule(..), updateViewCenter, manualViewCenterUpdate
  , GameState(..), initGameState

    -- ** Lenses

  , robotMap, newRobots, world, viewCenter, updated, replResult, inventory
  , messageQueue

    -- * Convenience re-exports

  , module Swarm.Game.Resource
  )
  where

import           Numeric.Noise.Perlin
-- import           Numeric.Noise.Ridged

import           Brick                   (AttrName)
import           Control.Arrow           ((&&&))
import           Control.Lens            hiding (Const, from)
import           Control.Monad.State
import           Data.List               (intercalate)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Linear
import           System.Random           (randomRIO)
import           Witch

-- import           Data.Hash.Murmur

import           Swarm.Game.Resource
import qualified Swarm.Game.World        as W
import           Swarm.Language.Pipeline
import           Swarm.Language.Pretty
import           Swarm.Language.Syntax
import           Swarm.Language.Types
import           Swarm.TUI.Attr
import           Swarm.Util

------------------------------------------------------------
-- CEK machine types
------------------------------------------------------------

-- | A /value/ is a term that cannot (or does not) take any more
--   evaluation steps on its own.
data Value where
  -- | The unit value.
  VUnit   :: Value

  -- | An integer.
  VInt    :: Integer -> Value

  -- | A literal string.
  VString :: Text -> Value

  -- | A direction.
  VDir    :: Direction -> Value

  -- | A boolean.
  VBool   :: Bool -> Value

  -- | A pair.
  VPair   :: Value -> Value -> Value

  -- | A /closure/, representing a lambda term along with an
  --   environment containing bindings for any free variables in the
  --   body of the lambda.
  VClo    :: Text -> UTerm -> Env -> Value

  -- | An application of a constant to some value arguments,
  --   potentially waiting for more arguments.
  VCApp   :: Const -> [Value] -> Value

  -- | A bind where the first component has been reduced to a value,
  --   /i.e./ @v ; c@ or @x <- v; c@.  We also store an @Env@ in which
  --   to interpret the second component of the bind.
  VBind   :: Value -> Maybe Var -> UTerm -> Env -> Value

  -- | A delayed term, along with its environment. If a term would
  --   otherwise be evaluated but we don't want it to be, we can stick
  --   a @delay@ on it, which turns it into a value.  Delayed terms
  --   won't be evaluated until @force@ is applied to them.
  VDelay  :: UTerm -> Env -> Value
  deriving (Eq, Ord, Show)

prettyValue :: Value -> Text
prettyValue = prettyText . valueToTerm

valueToTerm :: Value -> UTerm
valueToTerm VUnit            = TUnit
valueToTerm (VInt n)         = TInt n
valueToTerm (VString s)      = TString s
valueToTerm (VDir d)         = TDir d
valueToTerm (VBool b)        = TBool b
valueToTerm (VPair v1 v2)    = TPair (valueToTerm v1) (valueToTerm v2)
valueToTerm (VClo x t e)     =
  M.foldrWithKey
    (\y v -> TLet y NONE (valueToTerm v))
    (TLam x NONE t)
    (M.restrictKeys e (S.delete x (fv t)))
valueToTerm (VCApp c vs)     = foldl (TApp NONE) (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VBind v mx t _) = TBind mx NONE (valueToTerm v) t
valueToTerm (VDelay t _)     = TDelay t

-- | An environment is a mapping from variable names to values.
type Env = Map Var Value

-- | Unsafely look up variables in an environment, with a slightly
--   better error message just in case something goes wrong.  But in
--   theory, if the type checker is doing its job and there are no
--   bugs, a lookup error will never happen.
(!!!) :: Env -> Var -> Value
e !!! x = case M.lookup x e of
  Nothing -> error $ from x ++ " is not a key in the environment!"
  Just v  -> v

emptyEnv :: Env
emptyEnv = M.empty

-- | A frame is a single component of a continuation stack, explaining
--   what to do next after we finish evaluating the currently focused
--   term.
data Frame
   = FSnd UTerm Env
     -- ^ We were evaluating the fst component of a pair; next, we
     --   should evaluate the second component which was saved in this
     --   frame.

   | FFst Value
     -- ^ We were evaluating the snd component of a pair; when done,
     --   we should combine it with the value of the fst component saved
     --   in this frame to construct a fully evaluated pair.

   | FArg UTerm Env
    -- ^ @FArg t e@ says that we were evaluating the left-hand side of
    -- an application, so the next thing we should do is evaluate the
    -- term @t@ (the right-hand side, /i.e./ argument of the
    -- application) in environment @e@.  We will also push an 'FApp'
    -- frame on the stack.

  | FApp Value
    -- ^ @FApp v@ says that we were evaluating the right-hand side of
    -- an application; once we are done, we should pass the resulting
    -- value as an argument to @v@.

  | FLet Var UTerm Env
    -- ^ @FLet x t2 e@ says that we were evaluating a term @t1@ in an
    -- expression of the form @let x = t1 in t2@, that is, we were
    -- evaluating the definition of @x@; the next thing we should do
    -- is evaluate @t2@ in the environment @e@ extended with a binding
    -- for @x@.

  | FEvalBind (Maybe Text) UTerm Env
    -- ^ If the top frame is of the form @FEvalBind mx c2 e@, we were
    -- /evaluating/ a term @c1@ from a bind expression @x <- c1 ; c2@
    -- (or without the @x@, if @mx@ is @Nothing@); once finished, we
    -- should simply package it up into a value using @VBind@.

  | FExec
    -- ^ An @FExec@ frame means the focused value is a command, which
    -- we should now execute.

  | FExecBind (Maybe Text) UTerm Env
    -- ^ This looks very similar to 'FEvalBind', but it means we are
    -- in the process of /executing/ the first component of a bind;
    -- once done, we should also execute the second component in the
    -- given environment (extended by binding the variable, if there
    -- is one, to the output of the first command.

  deriving (Eq, Ord, Show)

-- | A continuation is just a stack of frames.
type Cont = [Frame]

-- | The overall state of a CEK machine, which can actually be in one
--   of two states. The CEK machine is named after the first kind of
--   state, and it would probably be possible to inline a bunch of
--   things and get rid of the second state, but I find it much more
--   natural and elegant this way.
data CEK
  = In UTerm Env Cont
    -- ^ When we are on our way "in/down" into a term, we have a
    --   currently focused term to evaluate in the environment, and a
    --   continuation.  In this mode we generally pattern-match on the
    --   'UTerm' to decide what to do next.

  | Out Value Cont
    -- ^ Once we finish evaluating a term, we end up with a 'Value'
    --   and we switch into "out/up" mode, bringing the value back up
    --   out of the depths to the context that was expecting it.  In
    --   this mode we generally pattern-match on the 'Cont' to decide
    --   what to do next.
    --
    --   Note that there is no 'Env', because we don't have any
    --   variables to evaluate at the moment, and we maintain the invariant
    --   that any unevaluated terms buried inside a 'Value' or 'Cont'
    --   must carry along their environment with them.
  deriving (Eq, Ord, Show)

-- | Is the CEK machine in a final (finished) state?
isFinal :: CEK -> Bool
isFinal (Out _ []) = True
isFinal _          = False

-- | Initialize a machine state with a starting term along with its
--   type, which will be executed or evaluated depending on whether it
--   has a command type or not.  It requires a fully typechecked term
--   (to make sure no type or scope errors can cause a crash), but
--   erases the term before putting it in the machine.
initMachine :: ATerm -> Type -> CEK
initMachine t (TyCmd _) = In (erase t) M.empty [FExec]
initMachine t _         = In (erase t) M.empty []

-- | A machine which does nothing.
idleMachine :: CEK
idleMachine = initMachine (TConst Noop) (TyCmd TyUnit)

-- | Initialize a machine state with a command that is already a value
--   (for example, this is the case when spawning a new robot with the
--   'build' command; because of eager evaluation, the argument to
--   'build' has already been evaluated (but not executed!).
initMachineV :: Value -> CEK
initMachineV v = Out v [FExec]

------------------------------------------------------------
-- FOR DEBUGGING ONLY, not exported.  Very crude pretty-printing of
-- CEK states.  Should really make a nicer version of this code...

prettyCEK :: CEK -> String
prettyCEK (In c _ k) = unlines
  [ "▶ " ++ prettyString c
  , "  " ++ prettyCont k ]
prettyCEK (Out v k) = unlines
  [ "◀ " ++ from (prettyValue v)
  , "  " ++ prettyCont k ]

prettyCont :: Cont -> String
prettyCont = ("["++) . (++"]") . intercalate " | " . map prettyFrame

prettyFrame :: Frame -> String
prettyFrame (FSnd t _)               = "(_, " ++ prettyString t ++ ")"
prettyFrame (FFst v)                 = "(" ++ from (prettyValue v) ++ ", _)"
prettyFrame (FArg t _)               = "_ " ++ prettyString t
prettyFrame (FApp v)                 = prettyString (valueToTerm v) ++ " _"
prettyFrame (FLet x t _)             = "let " ++ from x ++ " = _ in " ++ prettyString t
prettyFrame (FEvalBind Nothing t _)    = "_ ; " ++ prettyString t
prettyFrame (FEvalBind (Just x) t _)   = from x ++ " <- _ ; " ++ prettyString t
prettyFrame FExec                    = "exec _"
prettyFrame (FExecBind Nothing t _)  = "_ ; " ++ prettyString t
prettyFrame (FExecBind (Just x) t _) = from x ++ " <- _ ; " ++ prettyString t

-- END DEBUGGING CODE
------------------------------------------------------------

------------------------------------------------------------
-- Game state data types
------------------------------------------------------------

data RobotDisplay = RD
  { _defaultChar      :: Char
  , _dirMap           :: Map (V2 Int) Char
  , _robotDisplayAttr :: AttrName
  , _priority         :: Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''RobotDisplay

singleRobotDisplay :: Char -> RobotDisplay
singleRobotDisplay c = RD
  { _defaultChar = c
  , _dirMap = M.empty
  , _robotDisplayAttr = robotAttr
  , _priority = 10
  }

defaultRobotDisplay :: RobotDisplay
defaultRobotDisplay = RD
  { _defaultChar = '■'
  , _dirMap = M.fromList
      [ (east,  '▶')
      , (west,  '◀')
      , (south, '▼')
      , (north, '▲')
      ]
  , _robotDisplayAttr = robotAttr
  , _priority = 10
  }

lookupRobotDisplay :: V2 Int -> RobotDisplay -> Char
lookupRobotDisplay v rd = M.lookup v (rd ^. dirMap) ? (rd ^. defaultChar)

-- | A value of type 'Robot' is a record representing the state of a
--   single robot.
data Robot = Robot
  { _robotName    :: Text
    -- ^ The name of the robot (unique across the whole world)

  , _robotDisplay :: RobotDisplay

  , _location     :: V2 Int
    -- ^ The location of the robot as (row,col).

  , _direction    :: V2 Int
    -- ^ The direction of the robot as a 2D vector.  When the robot
    --   executes @move@, its 'location' is updated by adding
    --   'direction' to it.

  , _machine      :: CEK
    -- ^ The current state of the robot's CEK machine.

  , _tickSteps    :: Int
    -- ^ The need for 'tickSteps' is a bit technical, and I hope I can
    --   eventually find a different, better way to accomplish it.
    --   Ideally, we would want each robot to execute a single
    --   /command/ at every game tick, so that /e.g./ two robots
    --   executing @move;move;move@ and @repeat 3 move@ (given a
    --   suitable definition of @repeat@) will move in lockstep.
    --   However, the second robot actually has to do more computation
    --   than the first (it has to look up the definition of @repeat@,
    --   reduce its application to the number 3, etc.), so its CEK
    --   machine will take more steps.  It won't do to simply let each
    --   robot run until executing a command---because robot programs
    --   can involve arbitrary recursion, it is very easy to write a
    --   program that evaluates forever without ever executing a
    --   command, which in this scenario would completely freeze the
    --   UI. (It also wouldn't help to ensure all programs are
    --   terminating---it would still be possible to effectively do
    --   the same thing by making a program that takes a very, very
    --   long time to terminate.)  So instead, we allocate each robot
    --   a certain maximum number of computation steps per tick
    --   (defined in 'evalStepsPerTick'), and it suspends computation
    --   when it either executes a command or reaches the maximum
    --   number of steps, whichever comes first.
    --
    --   It seems like this really isn't something the robot should be
    --   keeping track of itself, but that seemed the most technically
    --   convenient way to do it at the time.  The robot needs some
    --   way to signal when it has executed a command, which it
    --   currently does by setting tickSteps to zero.  However, that
    --   has the disadvantage that when tickSteps becomes zero, we
    --   can't tell whether that happened because the robot ran out of
    --   steps, or because it executed a command and set it to zero
    --   manually.
    --
    --   Perhaps instead, each robot should keep a counter saying how
    --   many commands it has executed.  The loop stepping the robot
    --   can tell when the counter increments.

  , _static       :: Bool
    -- ^ Whether the robot is allowed to move, turn, or harvest.
    --   Currently this is a hack to prevent the "base" robot from
    --   moving; eventually this should go away, and the base will be
    --   prevented from moving simply because it does not have treads
    --   (or whatever the right device is for moving), and so on.
  }
  deriving (Eq, Ord, Show)

makeLenses ''Robot

-- | Create a robot.
mkRobot
  :: Text    -- ^ Name of the robot.  Precondition: it should not be the same as any
             --   other robot name.
  -> V2 Int  -- ^ Initial location.
  -> V2 Int  -- ^ Initial heading/direction.
  -> CEK     -- ^ Initial CEK machine.
  -> Robot
mkRobot name l d m = Robot
  { _robotName    = name
  , _robotDisplay = defaultRobotDisplay
  , _location     = l
  , _direction    = d
  , _machine      = m
  , _tickSteps    = 0
  , _static       = False
  }

-- | The initial robot representing your "base".
baseRobot :: Robot
baseRobot = Robot
  { _robotName    = "base"
  , _robotDisplay = singleRobotDisplay '■'
  , _location     = V2 0 0
  , _direction    = north
  , _machine      = idleMachine
  , _tickSteps    = 0
  , _static       = True
  }

-- | Is the robot actively in the middle of a computation?
isActive :: Robot -> Bool
isActive = not . isFinal . view machine

-- | Get the result of the machine if it is finished.
getResult :: Robot -> Maybe Value
getResult r = case r ^. machine of
  Out v [] -> Just v
  _        -> Nothing

data Item = Resource Char
  deriving (Eq, Ord, Show)

data ViewCenterRule
  = VCLocation (V2 Int)
  | VCRobot Text
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _robotMap       :: M.Map Text Robot
  , _newRobots      :: [Robot]
  , _gensym         :: Int
  , _world          :: W.TileCachingWorld
  , _viewCenterRule :: ViewCenterRule
  , _viewCenter     :: V2 Int
  , _updated        :: Bool
  , _replResult     :: Maybe (Type, Maybe Value)
  , _inventory      :: Map Item Int
  , _messageQueue   :: [Text]
  }

makeLenses ''GameState

applyViewCenterRule :: ViewCenterRule -> Map Text Robot -> Maybe (V2 Int)
applyViewCenterRule (VCLocation l) _ = Just l
applyViewCenterRule (VCRobot name) m = m ^? at name . _Just . location

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

pn1, pn2 :: Perlin
pn1 = perlin 0 5 0.05 0.5
pn2 = perlin 0 5 0.05 0.75

-- rn :: Ridged
-- rn = ridged 0 5 0.005 1 2

initGameState :: IO GameState
initGameState = return $
  GameState
  { _robotMap   = M.singleton "base" baseRobot
  , _newRobots  = []
  , _gensym     = 0
  , _world      = W.newWorld $ \(i,j) ->
      if noiseValue pn1 (fromIntegral i, fromIntegral j, 0) > 0
        then 'T'
        else
          if noiseValue pn2 (fromIntegral i, fromIntegral j, 0) > 0
            then '@'
            else '.'
--      if murmur3 0 (into (show (i + 3947*j))) `mod` 20 == 0 then '.' else ' '
  , _viewCenterRule = VCLocation (V2 0 0)
  , _viewCenter = V2 0 0
  , _updated    = False
  , _replResult = Nothing
  , _inventory  = M.empty
  , _messageQueue = []
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
--   the 'run' command; but eventually I want to get rid of that
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
  -- Doing it in three separate lines, as below, forces us to be
  -- explicit about the ordering of effects.  Note that in theory, the
  -- third line robotMap .= rm' could overwrite any effects to the
  -- robotMap that were generated by the second line; but in fact the
  -- robot stepping functions do not modify the robot map at all.
  rm <- use robotMap
  rm' <- M.traverseMaybeWithKey (const bigStepRobot) rm
  robotMap .= rm'

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

-- | The main CEK machine workhorse.  Given a robot, look at its CEK
--   machine state and figure out a single next step. The reason we
--   return a @Maybe Robot@ is that the robot could execute a 'halt'
--   instruction, making it disappear, which we signal by returning
--   @Nothing@.
stepRobot :: Robot -> StateT GameState IO (Maybe Robot)
stepRobot r = case r ^. machine of

  -- First a bunch of straightforward cases.  These are all
  -- immediately turned into values.
  In TUnit _ k                      -> step r $ Out VUnit k
  In (TDir d) _ k                   -> step r $ Out (VDir d) k
  In (TInt n) _ k                   -> step r $ Out (VInt n) k
  In (TString s) _ k                -> step r $ Out (VString s) k
  In (TBool b) _ k                  -> step r $ Out (VBool b) k

  -- A constant is turned into a VCApp which might be waiting for arguments.
  In (TConst c) _ k                 -> step r $ Out (VCApp c []) k

  -- To evaluate a variable, just look it up in the context.
  In (TVar x) e k                   -> step r $ Out (e !!! x) k

  -- XXX
  In (TPair t1 t2) e k              -> step r $ In t1 e (FSnd t2 e : k)

  -- Lambdas just immediately turn into closures.
  In (TLam x _ t) e k               -> step r $ Out (VClo x t e) k

  -- To evaluate an application, focus on the left-hand side and save
  -- the right-hand side for later.
  In (TApp _ t1 t2) e k             -> step r $ In t1 e (FArg t2 e : k)

  -- Evaluating let expressions is a little bit tricky. XXX write more
  In (TLet x _ t1 t2) e k           ->
    let e' = M.insert x (VDelay t1 e') e   -- XXX do this without making a recursive
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
  Out v2 (FApp (VClo x t e) : k)    -> step r $ In t (M.insert x v2 e) k
  Out v1 (FLet x t2 e : k)          -> step r $ In t2 (M.insert x v1 e) k
  Out v1 (FEvalBind mx t2 e : k)    -> step r $ Out (VBind v1 mx t2 e) k

  -- Special command applications that don't use up a tick (Noop and Return)
  Out (VCApp Noop _) (FExec : k)     -> step r $ Out VUnit k
  Out (VCApp Return [v]) (FExec : k) -> step r $ Out v k

  Out (VCApp c args) (FExec : k)    -> execConst c (reverse args) k (r & tickSteps .~ 0)
  Out (VBind c mx t2 e) (FExec : k) -> step r $ Out c (FExec : FExecBind mx t2 e : k)
  Out v (FExecBind mx t2 e : k)     -> step r $ In t2 (maybe id (`M.insert` v) mx e) (FExec : k)

  cek -> error $ "Panic! Bad machine state in stepRobot: " ++ show cek

nonStatic :: Const -> Cont -> Robot -> StateT GameState IO (Maybe Robot) -> StateT GameState IO (Maybe Robot)
nonStatic c k r m
  | r ^. static = do
      emitMessage $ T.concat ["Your base can't ", prettyText c, "!"]
      step r (Out VUnit k)
  | otherwise   = m

-- | At the level of the CEK machine there's no particular difference
--   between *evaluating* a function constant and *executing* a
--   command constant, but it somehow feels better to have two
--   different names for it anyway.
evalConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
evalConst = execConst

-- XXX load this from a file and have it available in a map?
--     Or make a quasiquoter?
seedProgram :: ATerm
Right (seedProgram ::: _) = processTerm . into @Text . unlines $
  [ "let repeat : int -> cmd () -> cmd () = \\n.\\c."
  , "  if (n == 0) {} {c ; repeat (n-1) c}"
  , "in {"
  , "  r <- random 500;"
  , "  repeat (r + 100) wait;"
  , "  appear \"|\";"
  , "  r <- random 500;"
  , "  repeat (r + 100) wait;"
  , "  halt"
  , "}"
  ]

execConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
execConst Wait _ k r   = step r $ Out VUnit k
execConst Halt _ _ _   = updated .= True >> return Nothing
execConst Return _ _ _ = error "execConst Return should have been handled already in stepRobot!"
execConst Noop _ _ _   = error "execConst Noop should have been handled already in stepRobot!"
execConst Move _ k r   = nonStatic Move k r $ do
  let V2 x y = (r ^. location) ^+^ (r ^. direction)
  resrc <- uses world (W.lookup (-y,x))
  let props = resourceMap ^. ix resrc . resProperties
  case Solid `S.member` props of
    True  -> step r (Out VUnit k)
    False -> do
      updated .= True
      step (r & location %~ (^+^ (r ^. direction))) (Out VUnit k)
execConst Harvest _ k r = nonStatic Harvest k r $ do
  updated .= True
  let V2 x y = r ^. location
  resrc <- uses world (W.lookup (-y,x))
  let props = resourceMap ^. ix resrc . resProperties
  case Harvestable `S.member` props of
    False -> step r $ Out VUnit k
    True -> do
      h <- uses world (W.lookup (-y,x))
      inventory . at (Resource h) . non 0 += 1
      let seedBot =
            mkRobot "seed" (r ^. location) (V2 0 0) (initMachine seedProgram (TyCmd TyUnit))
              & robotDisplay .~
                (singleRobotDisplay '.'
                 & robotDisplayAttr .~ plantAttr
                 & priority .~ 1
                )
      seedBot' <- ensureUniqueName seedBot
      newRobots %= (seedBot' :)
      step r (Out VUnit k)
execConst Turn [VDir d] k r = nonStatic Turn k r $ do
  updated .= True
  step (r & direction %~ applyTurn d) (Out VUnit k)
execConst Turn args k _ = badConst Turn args k

execConst GetX _ k r = step r $ Out (VInt (fromIntegral x)) k
  where
    V2 x _ = r ^. location
execConst GetY _ k r = step r $ Out (VInt (fromIntegral y)) k
  where
    V2 _ y = r ^. location

execConst Random [VInt hi] k r = do
  n <- randomRIO (0, hi-1)
  step r $ Out (VInt n) k
execConst Random args k _ = badConst Random args k

execConst Say [VString s] k r = do
  emitMessage (T.concat [r ^. robotName, ": ", s])
  step r $ Out VUnit k
execConst Say args k _ = badConst Say args k

execConst View [VString s] k r = do
  mr <- use (robotMap . at s)
  case mr of
    Nothing -> emitMessage $ T.concat["There is no robot named ", s, " to view."]
    Just _  -> viewCenterRule .= VCRobot s
  step r $ Out VUnit k
execConst View args k _ = badConst View args k

execConst Appear [VString s] k r = do
  updated .= True
  case into @String s of
    [c] ->
      step (r & robotDisplay . defaultChar .~ c
              & robotDisplay . dirMap .~ M.empty
           ) $ Out VUnit k
    [c,nc,ec,sc,wc] ->
      step ( r & robotDisplay . defaultChar .~ c
               & robotDisplay . dirMap . ix north .~ nc
               & robotDisplay . dirMap . ix east  .~ ec
               & robotDisplay . dirMap . ix south .~ sc
               & robotDisplay . dirMap . ix west  .~ wc
           ) $ Out VUnit k
    _ -> do
      emitMessage $ T.concat[s, " is not a valid appearance string."]
      step r $ Out VUnit k
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

execConst Build [VString name, c] k r = do
  let newRobot = mkRobot name (r ^. location) (r ^. direction) (initMachineV c)
  newRobot' <- ensureUniqueName newRobot
  newRobots %= (newRobot' :)
  updated .= True
  step r $ Out (VString (newRobot' ^. robotName)) k
execConst Build args k _ = badConst Build args k

execConst Run [VString fileName] k r = do
  f <- liftIO $ T.readFile (into fileName)  -- XXX handle file not existing
  case processCmd f of
    Left  err -> error (into err)  -- XXX, display message and do nothing
    Right t   -> step r $ In (erase t) M.empty (FExec : k)
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
