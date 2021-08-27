{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
  -- debugging code

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
    Value(..), Env, emptyEnv

    -- ** Frames

  , Frame(..), Cont

    -- ** CEK machine states

  , CEK(..), initMachine, initMachineV

    -- ** Stepping the machine

  , gameStep, step, evalStepsPerTick
  , bigStepRobot, stepRobot, execConst

    -- * Robots

  , Robot(..), mkRobot, mkBase

    -- ** Lenses
  , location, direction, machine, tickSteps, static
  , Item(..)

    -- * Game state
  , GameState(..), initGameState

    -- ** Lenses

  , robotMap, newRobots, world, viewCenter, updated, inventory

    -- * Convenience re-exports

  , module Swarm.Game.Resource
  )
  where

import           Numeric.Noise.Perlin
-- import           Numeric.Noise.Ridged

import           Control.Lens         hiding (Const, from)
import           Control.Monad.State
import           Data.List            (intercalate)
-- import           Data.Hash.Murmur
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Linear
import           Witch

import           Swarm.Pretty

import           Control.Arrow        ((&&&))
import           Swarm.AST
import           Swarm.Game.Resource
import qualified Swarm.Game.World     as W
import           Swarm.Util           (processCmd)

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
  = FArg UTerm Env
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

-- | Initialize a machine state with a starting command to execute,
--   requiring a fully typechecked term (to make sure no type or scope
--   errors can cause a crash), but erasing the term before putting it
--   in the machine.
initMachine :: ATerm -> CEK
initMachine e = In (erase e) M.empty [FExec]

-- | Initialize a machine state with a command that is already a value
--   (for example, this is the case when spawning a new robot with the
--   'build' command; because of eager evaluation, the argument to
--   'build' has already been evaluated (but not executed!).
initMachineV :: Value -> CEK
initMachineV v = Out v [FExec]

------------------------------------------------------------
-- FOR DEBUGGING ONLY
-- Should really make a nicer version of this code.

prettyCEK :: CEK -> String
prettyCEK (In c _ k) = unlines
  [ "▶ " ++ prettyString c
  , "  " ++ prettyCont k ]
prettyCEK (Out v k) = unlines
  [ "◀ " ++ prettyValue v
  , "  " ++ prettyCont k ]

prettyValue :: Value -> String
prettyValue = prettyString . valueToTerm

valueToTerm :: Value -> UTerm
valueToTerm VUnit            = TUnit
valueToTerm (VInt n)         = TInt n
valueToTerm (VString s)      = TString s
valueToTerm (VDir d)         = TDir d
valueToTerm (VBool b)        = TBool b
valueToTerm (VClo x t _)     = TLam x NONE t
valueToTerm (VCApp c vs)     = foldl (TApp NONE) (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VBind v mx t _) = TBind mx NONE (valueToTerm v) t
valueToTerm (VDelay t _)     = TDelay t

prettyCont :: Cont -> String
prettyCont = ("["++) . (++"]") . intercalate " | " . map prettyFrame

prettyFrame :: Frame -> String
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

data Robot = Robot
  { _robotName :: Text
  , _location  :: V2 Int
  , _direction :: V2 Int
  , _machine   :: CEK
  , _tickSteps :: Int
  , _static    :: Bool
  }
  deriving (Eq, Ord, Show)

makeLenses ''Robot

-- | Is the robot actively in the middle of a computation?
isActive :: Robot -> Bool
isActive = not . isFinal . view machine

mkRobot :: Text -> V2 Int -> V2 Int -> CEK -> Robot
mkRobot name l d m = Robot
  { _robotName = name
  , _location  = l
  , _direction = d
  , _machine   = m
  , _tickSteps = 0
  , _static    = False
  }

mkBase :: ATerm -> Robot
mkBase e = Robot
  { _robotName = "base"
  , _location  = V2 0 0
  , _direction = V2 0 1
  , _machine   = initMachine e
  , _tickSteps = 0
  , _static    = True
  }

data Item = Resource Char
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _robotMap   :: M.Map Text Robot
  , _newRobots  :: [Robot]
  , _world      :: W.TileCachingWorld
  , _viewCenter :: V2 Int
  , _updated    :: Bool
  , _inventory  :: Map Item Int
  }

makeLenses ''GameState

pn1, pn2 :: Perlin
pn1 = perlin 0 5 0.05 0.5
pn2 = perlin 0 5 0.05 0.75

-- rn :: Ridged
-- rn = ridged 0 5 0.005 1 2

initGameState :: IO GameState
initGameState = return $
  GameState
  { _robotMap   = M.singleton "base" (mkBase (TConst Noop))
  , _newRobots  = []
  , _world      = W.newWorld $ \(i,j) ->
      if noiseValue pn1 (fromIntegral i, fromIntegral j, 0) > 0
        then 'T'
        else
          if noiseValue pn2 (fromIntegral i, fromIntegral j, 0) > 0
            then 'O'
            else '.'
--      if murmur3 0 (into (show (i + 3947*j))) `mod` 20 == 0 then '.' else ' '
  , _viewCenter = V2 0 0
  , _updated    = False
  , _inventory  = M.empty
  }

------------------------------------------------------------
-- CEK machine

gameStep :: GameState -> IO GameState
gameStep = execStateT step

evalStepsPerTick :: Int
evalStepsPerTick = 100

step :: StateT GameState IO ()
step = do
  updated .= False

  rm <- use robotMap
  rm' <- M.traverseMaybeWithKey (const bigStepRobot) rm
  robotMap .= rm'

  -- XXX write why this doesn't work!
  {-
(19:42) <   byorgey> Does lens provide a combinator of type (something like)   MonadState s m => Lens' s a -> (a -> m a) -> m s  ?
(19:43)                -!- dsrt^ [~dsrt@12.16.129.111] has joined #haskell
(19:43) <   byorgey> I can do it with   get >>= theLens f   where   f :: a -> m a   ,  but wondered if there was already an operator to do this
(19:43)                -!- orhan89 [~orhan89@151.91.188.35.bc.googleusercontent.com] has joined #haskell
(19:44) <   byorgey> :t \lens f -> get >>= lens f
(19:44) < lambdabot> MonadState a m => (t -> a -> m b) -> t -> m b
(19:44)                -!- Codaraxis__ [~Codaraxis@user/codaraxis] has quit [Ping timeout: 240 seconds]
(19:46) <  hololeap> I found a solution to my problem that seems ok: make a new typeclass for semigroups/monoids that have a "short-circuit state", so that wrappers can know whether or not to evaluate the second argument to `sappend`: http://sprunge.us/hYNINa
(19:47) <   byorgey> In other words, I want to apply an update to the component of the monadic state targeted by the lens, but the update may itself have some effects in the monad
(19:48)                -!- hyiltiz [~quassel@31.220.5.250] has quit [Ping timeout: 240 seconds]
(19:48)                -!- hyiltiz [~quassel@31.220.5.250] has joined #haskell
(19:52) <   byorgey> wait, I'm not even sure  get >>= lens f  does what I want, because I think that throws out the returned state which has the updated thing in it.  Maybe it should be get >>= lens f >>= put.
(19:55)                -!- merijn [~merijn@83-160-49-249.ip.xs4all.nl] has joined #haskell
(19:57)                -!- azeem [~azeem@176.200.202.67] has quit [Ping timeout: 250 seconds]
(19:57)                -!- azeem [~azeem@176.200.202.67] has joined #haskell
(19:58) <   byorgey> oh, that doesn't work because the put overwrites any changes to the state made by the 'lens f' part!  Never mind, maybe there's no concise, lawful way to do this, because what if the effects of the 'lens f' part modify the part of the state
                     the lens is targeting?
(19:59) <   byorgey> In my situation that's not the case, but anyway, I will stick with something like   do { a <- use lens; a' <- f a; lens %= a' }

-}
  -- get >>= robotMap (M.traverseMaybeWithKey (const bigStepRobot)) >>= put

  new <- use newRobots
  robotMap %= M.union (M.fromList $ map (view robotName &&& id) new)
  newRobots .= []

bigStepRobot :: Robot -> StateT GameState IO (Maybe Robot)
bigStepRobot r
  | not (isActive r) || r ^. tickSteps <= 0 = return (Just r)
  | otherwise           = do
      r' <- stepRobot r
      maybe (return Nothing) (bigStepRobot . (tickSteps -~ 1)) r'

mkStep :: Robot -> CEK -> StateT GameState IO (Maybe Robot)
mkStep r cek = do
  -- liftIO $ appendFile "out.txt" (prettyCEK (r ^. machine))
  return . Just $ r & machine .~ cek

stepRobot :: Robot -> StateT GameState IO (Maybe Robot)
stepRobot r = case r ^. machine of
  In TUnit _ k                      -> mkStep r $ Out VUnit k
  In (TConst c) _ k                 -> mkStep r $ Out (VCApp c []) k
  In (TDir d) _ k                   -> mkStep r $ Out (VDir d) k
  In (TInt n) _ k                   -> mkStep r $ Out (VInt n) k
  In (TString s) _ k                -> mkStep r $ Out (VString s) k
  In (TBool b) _ k                  -> mkStep r $ Out (VBool b) k
  In (TVar x) e k                   -> mkStep r $ Out (e !!! x) k
  In (TLam x _ t) e k               -> mkStep r $ Out (VClo x t e) k
  In (TApp _ t1 t2) e k             -> mkStep r $ In t1 e (FArg t2 e : k)
  In (TLet x _ t1 t2) e k           ->
    let e' = M.insert x (VDelay t1 e') e
    in mkStep r $ In t1 e' (FLet x t2 e : k)
  In (TBind mx _ t1 t2) e k         -> mkStep r $ In t1 e (FEvalBind mx t2 e : k)
  In (TDelay t) e k                 -> mkStep r $ Out (VDelay t e) k

  Out _ []                          -> return (Just r)

  Out v1 (FArg t2 e : k)            -> mkStep r $ In t2 e (FApp v1 : k)
  Out v2 (FApp (VCApp c args) : k)
    | not (isCmd c) &&
      arity c == length args + 1    -> evalConst c (reverse (v2 : args)) k r
    | otherwise                     -> mkStep r $ Out (VCApp c (v2 : args)) k
  Out v2 (FApp (VClo x t e) : k)    -> mkStep r $ In t (M.insert x v2 e) k
  Out v1 (FLet x t2 e : k)          -> mkStep r $ In t2 (M.insert x v1 e) k
  Out v1 (FEvalBind mx t2 e : k)    -> mkStep r $ Out (VBind v1 mx t2 e) k
  Out (VCApp Noop _) (FExec : k)    -> mkStep r $ Out VUnit k
  Out (VCApp c args) (FExec : k)    -> execConst c (reverse args) k (r & tickSteps .~ 0)
  Out (VBind c mx t2 e) (FExec : k) -> mkStep r $ Out c (FExec : FExecBind mx t2 e : k)
  Out v (FExecBind mx t2 e : k)     -> mkStep r $ In t2 (maybe id (`M.insert` v) mx e) (FExec : k)

  cek -> error $ "Panic! Bad machine state in stepRobot: " ++ show cek

nonStatic :: Cont -> Robot -> StateT GameState IO (Maybe Robot) -> StateT GameState IO (Maybe Robot)
nonStatic k r m
  | r ^. static = mkStep r (Out VUnit k)  -- XXX message saying that the base can't move?
  | otherwise   = m

-- | At the level of the CEK machine there's no particular difference
--   between *evaluating* a function constant and *executing* a
--   command constant, but it somehow feels better to have two
--   different names for it anyway.
evalConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
evalConst = execConst

execConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
execConst Wait _ k r = mkStep r $ Out VUnit k
execConst Halt _ _ _ = updated .= True >> return Nothing
execConst Noop _ _ _ = error "execConst Noop should have been handled already in stepRobot!"
execConst Move _ k r = nonStatic k r $ do
  updated .= True
  mkStep (r & location %~ (^+^ (r ^. direction))) (Out VUnit k)
execConst Harvest _ k r = nonStatic k r $ do
  let V2 row col = r ^. location
  h <- uses world (W.lookup (row,col))
  world %= W.insert (row,col) ' '
  inventory . at (Resource h) . non 0 += 1
  mkStep r (Out VUnit k)
execConst Turn [VDir d] k r = nonStatic k r $ do
  updated .= True
  mkStep (r & direction %~ applyTurn d) (Out VUnit k)
execConst Turn args k _ = badConst Turn args k

execConst GetX _ k r = mkStep r $ Out (VInt (fromIntegral col)) k
  where
    V2 _ col = r ^. location
execConst GetY _ k r = mkStep r $ Out (VInt (fromIntegral (-row))) k
  where
    V2 row _ = r ^. location

execConst (Cmp c) [VInt n1, VInt n2] k r = mkStep r $ Out (VBool (evalCmp c n1 n2)) k
execConst (Cmp c) args k _ = badConst (Cmp c) args k

execConst (Arith c) [VInt n1, VInt n2] k r = mkStep r $ Out (VInt (evalArith c n1 n2)) k
execConst (Arith c) args k _ = badConst (Arith c) args k

execConst Force [VDelay t e] k r = mkStep r $ In t e k
execConst Force args k _ = badConst Force args k

  -- Note, if should evaluate the branches lazily, but since
  -- evaluation is eager, by the time we get here thn and els have
  -- already been fully evaluated --- what gives?  The answer is that
  -- we rely on elaboration to add 'lazy' wrappers around the branches
  -- (and a 'force' wrapper around the entire if).
execConst If [VBool True , thn, _] k r = mkStep r $ Out thn k
execConst If [VBool False, _, els] k r = mkStep r $ Out els k
execConst If args k _ = badConst If args k

execConst Build [VString name, c] k r = do
  newRobots %= (mkRobot name (r ^. location) (r ^. direction) (initMachineV c) :)
  updated .= True
  mkStep r (Out VUnit k)
execConst Build args k _ = badConst Build args k
execConst Run [VString fileName] k r = do
  f <- liftIO $ T.readFile (into fileName)  -- XXX handle file not existing
  case processCmd f of
    Left  err -> error (into err)  -- XXX, display message and do nothing
    Right t   -> mkStep r $ In (erase t) M.empty (FExec : k)
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
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = div
evalArith Exp = (^)
