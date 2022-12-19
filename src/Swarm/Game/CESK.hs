{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :  Swarm.Game.CESK
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The Swarm interpreter uses a technique known as a
-- <https://matt.might.net/articles/cesk-machines/ CESK machine> (if
-- you want to read up on them, you may want to start by reading about
-- <https://matt.might.net/articles/cek-machines/ CEK machines>
-- first).  Execution happens simply by iterating a step function,
-- sending one state of the CESK machine to the next. In addition to
-- being relatively efficient, this means we can easily run a bunch of
-- robots synchronously, in parallel, without resorting to any threads
-- (by stepping their machines in a round-robin fashion); pause and
-- single-step the game; save and resume, and so on.
--
-- Essentially, a CESK machine state has four components:
--
-- - The __C__ontrol is the thing we are currently focused on:
--   either a 'Term' to evaluate, or a 'Value' that we have
--   just finished evaluating.
-- - The __E__nvironment ('Env') is a mapping from variables that might
--   occur free in the Control to their values.
-- - The __S__tore ('Store') is a mapping from abstract integer
--   /locations/ to values.  We use it to store delayed (lazy) values,
--   so they will be computed at most once.
-- - The __K__ontinuation ('Cont') is a stack of 'Frame's,
--   representing the evaluation context, /i.e./ what we are supposed
--   to do after we finish with the currently focused thing.  When we
--   reduce the currently focused term to a value, the top frame on
--   the stack tells us how to proceed.
--
-- You can think of a CESK machine as a defunctionalization of a
-- recursive big-step interpreter, where we explicitly keep track of
-- the call stack and the environments that would be in effect at
-- various places in the recursion.  One could probably even derive
-- this mechanically, by writing a recursive big-step interpreter,
-- then converting it to CPS, then defunctionalizing the
-- continuations.
--
-- The slightly confusing thing about CESK machines is how we
-- have to pass around environments everywhere.  Basically,
-- anywhere there can be unevaluated terms containing free
-- variables (in values, in continuation stack frames, ...), we
-- have to store the proper environment alongside so that when
-- we eventually get around to evaluating it, we will be able to
-- pull out the environment to use.
module Swarm.Game.CESK (
  -- * Frames and continuations
  Frame (..),
  Cont,

  -- ** Wrappers for creating delayed change of state

  --    See 'FImmediate'.
  WorldUpdate (..),
  RobotUpdate (..),

  -- * Store
  Store,
  Addr,
  emptyStore,
  Cell (..),
  allocate,
  lookupCell,
  setCell,

  -- * CESK machine states
  CESK (..),

  -- ** Construction
  initMachine,
  initMachine',
  cancel,
  resetBlackholes,

  -- ** Extracting information
  finalValue,

  -- ** Pretty-printing
  prettyFrame,
  prettyCont,
  prettyCESK,
) where

import Control.Lens.Combinators (pattern Empty)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (intercalate)
import GHC.Generics (Generic)
import Swarm.Game.Entity (Entity, Inventory)
import Swarm.Game.Exception
import Swarm.Game.Value as V
import Swarm.Game.World (World)
import Swarm.Language.Context
import Swarm.Language.Pipeline
import Swarm.Language.Pretty
import Swarm.Language.Requirement (ReqCtx)
import Swarm.Language.Syntax
import Swarm.Language.Types
import Witch (from)

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
    --   'ReqCtx' to the robot's 'Swarm.Game.Robot.robotCtx'.
    FLoadEnv TCtx ReqCtx
  | -- | We were executing a definition; next we should take the resulting value
    --   and return a context binding the variable to the value.
    FDef Var
  | -- | An @FExec@ frame means the focused value is a command, which
    -- we should now execute.
    FExec
  | -- | We are in the process of executing the first component of a
    --   bind; once done, we should also execute the second component
    --   in the given environment (extended by binding the variable,
    --   if there is one, to the output of the first command).
    FBind (Maybe Var) Term Env
  | -- | XXX
    FDiscardEnv
  | -- | Apply specific updates to the world and current robot.
    FImmediate WorldUpdate RobotUpdate
  | -- | Update the memory cell at a certain location with the computed value.
    FUpdate Addr
  | -- | Signal that we are done with an atomic computation.
    FFinishAtomic
  | -- | We are in the middle of running a computation for all the
    --   nearby robots.  We have the function to run, and the list of
    --   robot IDs to run it on.
    FMeetAll Value [Int]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A continuation is just a stack of frames.
type Cont = [Frame]

------------------------------------------------------------
-- Store
------------------------------------------------------------

type Addr = Int

-- | 'Store' represents a store, indexing integer locations to 'Cell's.
data Store = Store {next :: Addr, mu :: IntMap Cell} deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | A memory cell can be in one of three states.
data Cell
  = -- | A cell starts out life as an unevaluated term together with
    --   its environment.
    E Term Env
  | -- | When the cell is 'Force'd, it is set to a 'Blackhole' while
    --   being evaluated.  If it is ever referenced again while still
    --   a 'Blackhole', that means it depends on itself in a way that
    --   would trigger an infinite loop, and we can signal an error.
    --   (Of course, we
    --   <http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html cannot
    --   detect /all/ infinite loops this way>.)
    --
    --   A 'Blackhole' saves the original 'Term' and 'Env' that are
    --   being evaluated; if Ctrl-C is used to cancel a computation
    --   while we are in the middle of evaluating a cell, the
    --   'Blackhole' can be reset to 'E'.
    Blackhole Term Env
  | -- | Once evaluation is complete, we cache the final 'Value' in
    --   the 'Cell', so that subsequent lookups can just use it
    --   without recomputing anything.
    V Value
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

emptyStore :: Store
emptyStore = Store 0 IM.empty

-- | Allocate a new memory cell containing an unevaluated expression
--   with the current environment.  Return the index of the allocated
--   cell.
allocate :: Env -> Term -> Store -> (Addr, Store)
allocate e t (Store n m) = (n, Store (n + 1) (IM.insert n (E t e) m))

-- | Look up the cell at a given index.
lookupCell :: Addr -> Store -> Maybe Cell
lookupCell n = IM.lookup n . mu

-- | Set the cell at a given index.
setCell :: Addr -> Cell -> Store -> Store
setCell n c (Store nxt m) = Store nxt (IM.insert n c m)

------------------------------------------------------------
-- CESK machine
------------------------------------------------------------

-- | The overall state of a CESK machine, which can actually be one of
--   three kinds of states. The CESK machine is named after the first
--   kind of state, and it would probably be possible to inline a
--   bunch of things and get rid of the second state, but I find it
--   much more natural and elegant this way.  Most tutorial
--   presentations of CEK/CESK machines only have one kind of state, but
--   then again, most tutorial presentations only deal with the bare
--   lambda calculus, so one can tell whether a term is a value just
--   by seeing whether it is syntactically a lambda.  I learned this
--   approach from Harper's Practical Foundations of Programming
--   Languages.
data CESK
  = -- | When we are on our way "in/down" into a term, we have a
    --   currently focused term to evaluate in the environment, a store,
    --   and a continuation.  In this mode we generally pattern-match on the
    --   'Term' to decide what to do next.
    In Term Env Store Cont
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
    Out Value Store Cont
  | -- | An exception has been raised.  Keep unwinding the
    --   continuation stack (until finding an enclosing 'Try' in the
    --   case of a command failure or a user-generated exception, or
    --   until the stack is empty in the case of a fatal exception).
    Up Exn Store Cont
  | -- | The machine is waiting for the game to reach a certain time
    --   to resume its execution.
    Waiting Integer CESK
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Is the CESK machine in a final (finished) state?  If so, extract
--   the final value and store.
finalValue :: CESK -> Maybe (Value, Store)
{-# INLINE finalValue #-}
finalValue (Out v s []) = Just (v, s)
finalValue _ = Nothing

-- | Initialize a machine state with a starting term along with its
--   type; the term will be executed or just evaluated depending on
--   whether it has a command type or not.
initMachine :: ProcessedTerm -> Env -> Store -> CESK
initMachine t e s = initMachine' t e s []

-- | Like 'initMachine', but also take an explicit starting continuation.
initMachine' :: ProcessedTerm -> Env -> Store -> Cont -> CESK
initMachine' (ProcessedTerm t (Module (Forall _ (TyCmd _)) ctx) _ reqCtx) e s k =
  case ctx of
    Empty -> In t e s (FExec : k)
    _ -> In t e s (FExec : FLoadEnv ctx reqCtx : k)
initMachine' (ProcessedTerm t _ _ _) e s k = In t e s k

-- | Cancel the currently running computation.
cancel :: CESK -> CESK
cancel cesk = Out VUnit s' []
 where
  s' = resetBlackholes $ getStore cesk
  getStore (In _ _ s _) = s
  getStore (Out _ s _) = s
  getStore (Up _ s _) = s
  getStore (Waiting _ c) = getStore c

-- | Reset any 'Blackhole's in the 'Store'.  We need to use this any
--   time a running computation is interrupted, either by an exception
--   or by a Ctrl+C.
resetBlackholes :: Store -> Store
resetBlackholes (Store n m) = Store n (IM.map resetBlackhole m)
 where
  resetBlackhole (Blackhole t e) = E t e
  resetBlackhole c = c

------------------------------------------------------------
-- Very crude pretty-printing of CESK states.  Should really make a
-- nicer version of this code...
------------------------------------------------------------

-- | Very poor pretty-printing of CESK machine states, really just for
--   debugging. At some point we should make a nicer version.
prettyCESK :: CESK -> String
prettyCESK (In c _ _ k) =
  unlines
    [ "▶ " ++ prettyString c
    , "  " ++ prettyCont k
    ]
prettyCESK (Out v _ k) =
  unlines
    [ "◀ " ++ from (prettyValue v)
    , "  " ++ prettyCont k
    ]
prettyCESK (Up e _ k) =
  unlines
    [ "! " ++ from (formatExn mempty e)
    , "  " ++ prettyCont k
    ]
prettyCESK (Waiting t cek) =
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
prettyFrame (FTry t) = "try _ (" ++ prettyString (valueToTerm t) ++ ")"
prettyFrame FUnionEnv {} = "_ ∪ <Env>"
prettyFrame FLoadEnv {} = "loadEnv"
prettyFrame (FDef x) = "def " ++ from x ++ " = _"
prettyFrame FExec = "exec _"
prettyFrame (FBind Nothing t _) = "_ ; " ++ prettyString t
prettyFrame (FBind (Just x) t _) = from x ++ " <- _ ; " ++ prettyString t
prettyFrame FImmediate {} = "(_ : cmd a)"
prettyFrame (FUpdate loc) = "store@" ++ show loc ++ "(_)"
prettyFrame FFinishAtomic = "finishAtomic"
prettyFrame (FMeetAll _f _rs) = "meetAll"

--------------------------------------------------------------
-- Wrappers for functions in FImmediate
--
-- NOTE: we can not use GameState and Robot directly, as it
-- would create a cyclic dependency. The alternative is
-- making CESK, Cont and Frame polymorphic which just muddies
-- the picture too much for one little game feature.
--
-- BEWARE: the types do not follow normal laws for Show and Eq
--------------------------------------------------------------

newtype WorldUpdate = WorldUpdate
  { worldUpdate :: World Int Entity -> Either Exn (World Int Entity)
  }

newtype RobotUpdate = RobotUpdate
  { robotUpdateInventory :: Inventory -> Inventory
  }

instance Show WorldUpdate where show _ = "WorldUpdate {???}"

instance Show RobotUpdate where show _ = "RobotUpdate {???}"

instance Eq WorldUpdate where _ == _ = True

instance Eq RobotUpdate where _ == _ = True

-- TODO: remove these instances once Update fields are concret
instance FromJSON WorldUpdate where parseJSON _ = pure $ WorldUpdate $ \w -> Right w
instance ToJSON WorldUpdate where toJSON _ = Data.Aeson.Null
instance FromJSON RobotUpdate where parseJSON _ = pure $ RobotUpdate id
instance ToJSON RobotUpdate where toJSON _ = Data.Aeson.Null
