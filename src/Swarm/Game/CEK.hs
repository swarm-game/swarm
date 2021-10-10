-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Swarm.Game.CEK
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The Swarm interpreter uses a technique known as a
-- <https://matt.might.net/articles/cek-machines/ CEK machine>.
-- Execution happens simply by iterating a step function,
-- sending one state of the CEK machine to the next. In addition
-- to being relatively efficient, this means we can easily run a
-- bunch of robots synchronously, in parallel, without resorting
-- to any threads (by stepping their machines in a round-robin
-- fashion); pause and single-step the game; save and resume,
-- and so on.
--
-- Essentially, a CEK machine state has three components:
--
-- - The __C__ontrol is the thing we are currently focused on:
--   either a 'Term' to evaluate, or a 'Value' that we have
--   just finished evaluating.
-- - The __E__nvironment ('Env') is a mapping from variables that might
--   occur free in the Control to their values.
-- - The __K__ontinuation ('Cont') is a stack of 'Frame's,
--   representing the evaluation context, /i.e./ what we are supposed
--   to do after we finish with the currently focused thing.  When we
--   reduce the currently focused term to a value, the top frame on
--   the stack tells us how to proceed.
--
-- You can think of a CEK machine as a defunctionalization of a
-- recursive big-step interpreter, where we explicitly keep track of
-- the call stack and the environments that would be in effect at
-- various places in the recursion.  One could probably even derive
-- this mechanically, by writing a recursive big-step interpreter,
-- then converting it to CPS, then defunctionalizing the
-- continuations.
--
-- The slightly confusing thing about CEK machines is how we
-- have to pass around environments everywhere.  Basically,
-- anywhere there can be unevaluated terms containing free
-- variables (in values, in continuation stack frames, ...), we
-- have to store the proper environment alongside so that when
-- we eventually get around to evaluating it, we will be able to
-- pull out the environment to use.
module Swarm.Game.CEK (
  -- * Frames and continuations
  Frame (..),
  Cont,

  -- ** Wrappers for creating delayed change of state

  --    See 'FImmediate'.
  WorldFun (..),
  RobotFun (..),

  -- * CEK machine states
  CEK (..),

  -- ** Construction
  initMachine,
  initMachine',
  idleMachine,

  -- ** Extracting information
  finalValue,

  -- ** Pretty-printing
  prettyFrame,
  prettyCont,
  prettyCEK,
) where

import Control.Lens.Combinators (pattern Empty)
import Data.List (intercalate)
import qualified Data.Set as S
import Witch (from)

import Swarm.Game.Entity (Entity, Inventory)
import Swarm.Game.Exception
import Swarm.Game.Value as V
import Swarm.Game.World (World)
import Swarm.Language.Capability (CapCtx)
import Swarm.Language.Context
import Swarm.Language.Pipeline
import Swarm.Language.Pretty
import Swarm.Language.Syntax
import Swarm.Language.Types

------------------------------------------------------------
-- Frames and continuations
------------------------------------------------------------

-- | A frame is a single component of a continuation stack, explaining
--   what to do next after we finish evaluating the currently focused
--   term.
data Frame
  = -- | We were evaluating the first component of a pair; next, we
    --   should evaluate the second component which was saved in this
    --   frame (and push a 'FFst' frame on the stack to save the first component).
    FSnd Term Env
  | -- | We were evaluating the second component of a pair; when done,
    --   we should combine it with the value of the first component saved
    --   in this frame to construct a fully evaluated pair.
    FFst Value
  | -- | @FArg t e@ says that we were evaluating the left-hand side of
    -- an application, so the next thing we should do is evaluate the
    -- term @t@ (the right-hand side, /i.e./ argument of the
    -- application) in environment @e@.  We will also push an 'FApp'
    -- frame on the stack.
    FArg Term Env
  | -- | @FApp v@ says that we were evaluating the right-hand side of
    -- an application; once we are done, we should pass the resulting
    -- value as an argument to @v@.
    FApp Value
  | -- | @FLet x t2 e@ says that we were evaluating a term @t1@ in an
    -- expression of the form @let x = t1 in t2@, that is, we were
    -- evaluating the definition of @x@; the next thing we should do
    -- is evaluate @t2@ in the environment @e@ extended with a binding
    -- for @x@.
    FLet Var Term Env
  | -- | We are executing inside a 'Try' block.  If an exception is
    --   raised, we will execute the stored term (the "catch" block).
    FTry Value
  | -- | We were executing a command; next we should take any
    --   environment it returned and union it with this one to produce
    --   the result of a bind expression.
    FUnionEnv Env
  | -- | We were executing a command that might have definitions; next
    --   we should take the resulting 'Env' and add it to the robot's
    --   'Swarm.Game.Robot.robotEnv', along with adding this accompanying 'Ctx' and
    --   'CapCtx' to the robot's 'Swarm.Game.Robot.robotCtx'.
    FLoadEnv TCtx CapCtx
  | -- | An @FExec@ frame means the focused value is a command, which
    -- we should now execute.
    FExec
  | -- | We are in the process of executing the first component of a
    --   bind; once done, we should also execute the second component
    --   in the given environment (extended by binding the variable,
    --   if there is one, to the output of the first command).
    FBind (Maybe Var) Term Env
  | FImmediate WorldFun RobotFun
  deriving (Eq, Show)

-- | A continuation is just a stack of frames.
type Cont = [Frame]

-- | The overall state of a CEK machine, which can actually be one of
--   three kinds of states. The CEK machine is named after the first
--   kind of state, and it would probably be possible to inline a
--   bunch of things and get rid of the second state, but I find it
--   much more natural and elegant this way.  Most tutorial
--   presentations of CEK machines only have one kind of state, but
--   then again, most tutorial presentations only deal with the bare
--   lambda calculus, so one can tell whether a term is a value just
--   by seeing whether it is syntactically a lambda.  I learned this
--   approach from Harper's Practical Foundations of Programming
--   Languages.
data CEK
  = -- | When we are on our way "in/down" into a term, we have a
    --   currently focused term to evaluate in the environment, and a
    --   continuation.  In this mode we generally pattern-match on the
    --   'Term' to decide what to do next.
    In Term Env Cont
  | -- | Once we finish evaluating a term, we end up with a 'Value'
    --   and we switch into "out" mode, bringing the value back up
    --   out of the depths to the context that was expecting it.  In
    --   this mode we generally pattern-match on the 'Cont' to decide
    --   what to do next.
    --
    --   Note that there is no 'Env', because we don't have anything
    --   with variables to evaluate at the moment, and we maintain the
    --   invariant that any unevaluated terms buried inside a 'Value'
    --   or 'Cont' must carry along their environment with them.
    Out Value Cont
  | -- | An exception has been raised.  Keep unwinding the
    --   continuation stack (until finding an enclosing 'Try' in the
    --   case of a command failure or a user-generated exception, or
    --   until the stack is empty in the case of a fatal exception).
    Up Exn Cont
  | -- | The machine is waiting for the game to reach a certain time
    --   to resume its execution.
    Waiting Integer CEK
  deriving (Eq, Show)

-- | Is the CEK machine in a final (finished) state?  If so, extract
--   the final value.
finalValue :: CEK -> Maybe Value
{-# INLINE finalValue #-}
finalValue (Out v []) = Just v
finalValue _ = Nothing

-- | Initialize a machine state with a starting term along with its
--   type; the term will be executed or just evaluated depending on
--   whether it has a command type or not.
initMachine :: ProcessedTerm -> Env -> CEK
initMachine t e = initMachine' t e []

-- | Like 'initMachine', but also take a starting continuation.
initMachine' :: ProcessedTerm -> Env -> Cont -> CEK
initMachine' (ProcessedTerm t (Module (Forall _ (TyCmd _)) ctx) _ capCtx) e k =
  case ctx of
    Empty -> In t e (FExec : k)
    _ -> In t e (FExec : FLoadEnv ctx capCtx : k)
initMachine' (ProcessedTerm t _ _ _) e k = In t e k

-- | A machine which does nothing.
idleMachine :: CEK
idleMachine = initMachine trivialTerm empty
 where
  trivialTerm =
    ProcessedTerm
      (TConst Noop)
      (trivMod (Forall [] (TyCmd TyUnit)))
      S.empty
      empty

------------------------------------------------------------
-- Very crude pretty-printing of CEK states.  Should really make a
-- nicer version of this code...
------------------------------------------------------------

-- | Very poor pretty-printing of CEK machine states, really just for
--   debugging. At some point we should make a nicer version.
prettyCEK :: CEK -> String
prettyCEK (In c _ k) =
  unlines
    [ "▶ " ++ prettyString c
    , "  " ++ prettyCont k
    ]
prettyCEK (Out v k) =
  unlines
    [ "◀ " ++ from (prettyValue v)
    , "  " ++ prettyCont k
    ]
prettyCEK (Up e k) =
  unlines
    [ "! " ++ from (formatExn e)
    , "  " ++ prettyCont k
    ]
prettyCEK (Waiting t cek) =
  "🕑" <> show t <> " " <> show cek

-- | Poor pretty-printing of continuations.
prettyCont :: Cont -> String
prettyCont = ("[" ++) . (++ "]") . intercalate " | " . map prettyFrame

-- | Poor pretty-printing of frames.
prettyFrame :: Frame -> String
prettyFrame (FSnd t _) = "(_, " ++ prettyString t ++ ")"
prettyFrame (FFst v) = "(" ++ from (prettyValue v) ++ ", _)"
prettyFrame (FArg t _) = "_ " ++ prettyString t
prettyFrame (FApp v) = prettyString (valueToTerm v) ++ " _"
prettyFrame (FLet x t _) = "let " ++ from x ++ " = _ in " ++ prettyString t
prettyFrame (FTry c) = "try _ (" ++ from (prettyValue c) ++ ")"
prettyFrame FUnionEnv {} = "_ ∪ <Env>"
prettyFrame FLoadEnv {} = "loadEnv"
prettyFrame FExec = "exec _"
prettyFrame (FBind Nothing t _) = "_ ; " ++ prettyString t
prettyFrame (FBind (Just x) t _) = from x ++ " <- _ ; " ++ prettyString t
prettyFrame FImmediate {} = "(_ : cmd a)"

--------------------------------------------------------------
-- Wrappers for functions in FImmediate
--
-- NOTE: we can not use GameState and Robot directly, as it
-- would create a cyclic dependency. The alternative is
-- making CEK, Cont and Frame polymorphic which just muddies
-- the picture too much for one little game feature.
--
-- BEWARE: the types do not follow normal laws for Show and Eq
--------------------------------------------------------------

newtype WorldFun = WorldFun
  { worldFun :: World Int Entity -> Either Exn (World Int Entity)
  }

newtype RobotFun = RobotFun
  { robotFunInventory :: Inventory -> Inventory
  }

instance Show WorldFun where show _ = "WorldFun {???}"

instance Show RobotFun where show _ = "RobotFun {???}"

instance Eq WorldFun where _ == _ = True

instance Eq RobotFun where _ == _ = True
