{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: State machine of Swarm's interpreter
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
  WorldUpdate (..),
  RobotUpdate (..),

  -- * Store
  Store,
  Addr,
  emptyStore,
  allocate,
  resolveValue,
  lookupStore,
  setStore,

  -- * CESK machine states
  CESK (..),

  -- ** Construction
  initMachine,
  continue,
  cancel,
  prepareTerm,

  -- ** Extracting information
  finalValue,
  suspendedEnv,
  store,
  cont,
) where

import Control.Lens (Lens', Traversal', lens, traversal, (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty (..), encloseSep, hsep, (<+>))
import Swarm.Game.Entity (Entity)
import Swarm.Game.Exception
import Swarm.Game.Ingredients (Count)
import Swarm.Game.Tick
import Swarm.Game.World (WorldUpdate (..))
import Swarm.Language.Elaborate (insertSuspend)
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Syntax
import Swarm.Language.Types
import Swarm.Language.Value as V
import Swarm.Pretty (PrettyPrec (..), pparens, ppr)
import Swarm.Util.JSON (optionsMinimize)

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
  | -- | @FVArg v@ says that we were evaluating the left-hand side of
    --   an application, and the next thing we should do is apply it
    --   to the given value.  This does not normally occur as part of
    --   the usual evaluation process for applications, which instead
    --   uses FArg.  However, it is sometimes useful when reducing
    --   other constructs---for example, the pair eliminator 'match'.
    FVArg Value
  | -- | @FApp v@ says that we were evaluating the right-hand side of
    -- an application; once we are done, we should pass the resulting
    -- value as an argument to @v@.
    FApp Value
  | -- | @FLet x ty t2 e@ says that we were evaluating a term @t1@ of
    -- type @ty@ in an expression of the form @let x = t1 in t2@, that
    -- is, we were evaluating the definition of @x@; the next thing we
    -- should do is evaluate @t2@ in the environment @e@ extended with
    -- a binding for @x@.
    FLet Var (Maybe (Polytype, Requirements)) Term Env
  | -- | We are executing inside a 'Try' block.  If an exception is
    --   raised, we will execute the stored term (the "catch" block).
    FTry Value
  | -- | An @FExec@ frame means the focused value is a command, which
    -- we should now execute.
    FExec
  | -- | We are in the process of executing the first component of a
    --   bind; once done, we should also execute the second component
    --   in the given environment (extended by binding the variable,
    --   if there is one, to the output of the first command).
    FBind (Maybe Var) (Maybe (Polytype, Requirements)) Term Env
  | -- | Apply specific updates to the world and current robot.
    --
    -- The 'Const' is used to track the original command for error messages.
    FImmediate Const [WorldUpdate Entity] [RobotUpdate]
  | -- | Update the cell at a certain location in the store with the computed value.
    FUpdate Addr
  | -- | Signal that we are done with an atomic computation.
    FFinishAtomic
  | -- | We are in the middle of evaluating a record: some fields have
    --   already been evaluated; we are focusing on evaluating one
    --   field; and some fields have yet to be evaluated.
    FRcd Env [(Var, Value)] Var [(Var, Maybe Term)]
  | -- | We are in the middle of evaluating a record field projection.
    FProj Var
  | -- | We should suspend with the given environment once we finish
    --   the current evaluation.
    FSuspend Env
  | -- | If an exception bubbles all the way up to this frame, then
    --   switch to Suspended mode with this saved top-level context.
    FRestoreEnv Env
  deriving (Generic)

instance ToJSON Frame where
  toJSON = genericToJSON optionsMinimize

-- | A continuation is just a stack of frames.
type Cont = [Frame]

------------------------------------------------------------
-- Store
------------------------------------------------------------

type Addr = Int

-- | 'Store' represents a store, /i.e./ memory, indexing integer
--   locations to 'Value's.
data Store = Store {next :: Addr, mu :: IntMap Value}
  deriving (Generic, ToJSON)

emptyStore :: Store
emptyStore = Store 0 IM.empty

-- | Allocate a new memory cell containing a given value.  Return the
--   index of the allocated cell.
allocate :: Value -> Store -> (Addr, Store)
allocate v (Store n m) = (n, Store (n + 1) (IM.insert n v m))

-- | Resolve a value, recursively looking up any indirections in the
--   store.
resolveValue :: Store -> Value -> Either Addr Value
resolveValue s = \case
  VIndir loc -> lookupStore s loc
  v -> Right v

-- | Look up the value at a given index, but keep following
--   indirections until encountering a value that is not a 'VIndir'.
lookupStore :: Store -> Addr -> Either Addr Value
lookupStore s = go
 where
  go loc = case IM.lookup loc (mu s) of
    Nothing -> Left loc
    Just v -> case v of
      VIndir loc' -> go loc'
      _ -> Right v

-- | Set the value at a given index.
setStore :: Addr -> Value -> Store -> Store
setStore n c (Store nxt m) = Store nxt (IM.insert n c m)

------------------------------------------------------------
-- CESK machine
------------------------------------------------------------

-- | The overall state of a CESK machine, which can actually be one of
--   four kinds of states. The CESK machine is named after the first
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
    Waiting TickNumber CESK
  | -- | The machine is suspended, i.e. waiting for another term to
    --   evaluate.  This happens after we have evaluated whatever the
    --   user entered at the REPL and we are waiting for them to type
    --   something else.  Conceptually, this is like a combination of
    --   'Out' and 'In': we store a 'Value' that was just yielded by
    --   evaluation, and otherwise it is just like 'In' with a hole
    --   for the 'Term' we are going to evaluate.
    Suspended Value Env Store Cont
  deriving (Generic)

instance ToJSON CESK where
  toJSON = genericToJSON optionsMinimize

-- | Is the CESK machine in a final (finished) state?  If so, extract
--   the final value and store.
finalValue :: CESK -> Maybe Value
{-# INLINE finalValue #-}
finalValue (Out v _ []) = Just v
finalValue (Suspended v _ _ []) = Just v
finalValue _ = Nothing

-- | Extract the environment from a suspended CESK machine (/e.g./ to
--   use for typechecking).
suspendedEnv :: Traversal' CESK Env
suspendedEnv = traversal go
 where
  go :: Applicative f => (Env -> f Env) -> CESK -> f CESK
  go f (Suspended v e s k) = Suspended v <$> f e <*> pure s <*> pure k
  go _ cesk = pure cesk

-- | Lens focusing on the store of a CESK machine.
store :: Lens' CESK Store
store = lens get set
 where
  get = \case
    In _ _ s _ -> s
    Out _ s _ -> s
    Up _ s _ -> s
    Waiting _ c -> get c
    Suspended _ _ s _ -> s
  set cesk s = case cesk of
    In t e _ k -> In t e s k
    Out v _ k -> Out v s k
    Up x _ k -> Up x s k
    Waiting t c -> Waiting t (set c s)
    Suspended v e _ k -> Suspended v e s k

-- | Lens focusing on the continuation of a CESK machine.
cont :: Lens' CESK Cont
cont = lens get set
 where
  get = \case
    In _ _ _ k -> k
    Out _ _ k -> k
    Up _ _ k -> k
    Waiting _ c -> get c
    Suspended _ _ _ k -> k
  set cesk k = case cesk of
    In t e s _ -> In t e s k
    Out v s _ -> Out v s k
    Up x s _ -> Up x s k
    Waiting t c -> Waiting t (set c k)
    Suspended v e s _ -> Suspended v e s k

-- | Create a brand new CESK machine, with empty environment and
--   store, to evaluate a given term.  We always initialize the
--   machine with a single FExec frame as the continuation; if the
--   given term does not have a command type, we wrap it in @pure@.
initMachine :: TSyntax -> CESK
initMachine t = In (prepareTerm V.emptyEnv t) V.emptyEnv emptyStore [FExec]

-- | Load a program into an existing robot CESK machine: either
--   continue from a suspended state, or, as a fallback, start from
--   scratch with an empty environment but the same store.
--
--   Also insert a @suspend@ primitive at the end, so the resulting
--   term is suitable for execution by the base (REPL) robot.
continue :: TSyntax -> CESK -> CESK
continue t = \case
  -- The normal case is when we are continuing from a suspended state. We:
  --
  --   (1) insert a suspend call at the end of the term, so that in
  --   the normal case after executing the entire term we will suspend
  --   in the innermost scope, to continue executing another term
  --   within that scope later.
  --
  --   (2) insert a failsafe FRestoreEnv frame into the continuation
  --   stack, in case execution of the term throws an exception.  In
  --   that case we will fall back to suspending with the original
  --   environment e (any names brought into scope by executing the
  --   term will be discarded).  If the term succeeds, the extra
  --   FRestoreEnv frame will be discarded.
  Suspended _ e s k -> In (insertSuspend $ prepareTerm e t) e s (FExec : FRestoreEnv e : k)
  -- In any other state, just start with an empty environment.  This
  -- happens e.g. when running a program on the base robot for the
  -- very first time.
  cesk -> In (insertSuspend $ prepareTerm V.emptyEnv t) V.emptyEnv (cesk ^. store) (FExec : (cesk ^. cont))

-- | Prepare a term for evaluation by a CESK machine in the given
--   environment: erase all type annotations, and optionally wrap it
--   in @pure@ if it does not have a command type.  Note that since
--   the environment might contain type aliases, we have to be careful
--   to expand them before concluding whether the term has a command
--   type or not.
prepareTerm :: Env -> TSyntax -> Term
prepareTerm e t = case whnfType (e ^. envTydefs) (ptBody (t ^. sType)) of
  TyCmd _ -> t'
  _ -> TApp (TConst Pure) t'
 where
  t' = eraseS t

-- | Cancel the currently running computation.
cancel :: CESK -> CESK
cancel cesk = Up Cancel (cesk ^. store) (cesk ^. cont)

------------------------------------------------------------
-- Pretty printing CESK machine states
------------------------------------------------------------

instance PrettyPrec CESK where
  prettyPrec _ = \case
    In c _ _ k -> prettyCont k (11, "▶" <> ppr c <> "◀")
    Out v _ k -> prettyCont k (11, "◀" <> ppr (valueToTerm v) <> "▶")
    Up e _ k -> prettyCont k (11, "!" <> (pretty (formatExn mempty e) <> "!"))
    Waiting t cesk -> "🕑" <> pretty t <> "(" <> ppr cesk <> ")"
    Suspended v _ _ k -> prettyCont k (11, "◀" <> ppr (valueToTerm v) <> "...▶")

-- | Take a continuation, and the pretty-printed expression which is
--   the focus of the continuation (i.e. the expression whose value
--   will be given to the continuation) along with its top-level
--   precedence, and pretty-print the whole thing.
--
--   As much as possible, we try to print to look like an *expression*
--   with a currently focused part, that is, we print the continuation
--   from the inside out instead of as a list of frames.  This makes
--   it much more intuitive to read.
prettyCont :: Cont -> (Int, Doc ann) -> Doc ann
prettyCont [] (_, inner) = inner
prettyCont (f : k) inner = prettyCont k (prettyFrame f inner)

-- | Pretty-print a single continuation frame, given its already
--   pretty-printed focus.  In particular, given a frame and its
--   "inside" (i.e. the expression or other frames being focused on,
--   whose value will eventually be passed to this frame), with the
--   precedence of the inside's top-level construct, return a
--   pretty-printed version of the entire frame along with its
--   top-level precedence.
prettyFrame :: Frame -> (Int, Doc ann) -> (Int, Doc ann)
prettyFrame f (p, inner) = case f of
  FSnd t _ -> (11, "(" <> inner <> "," <+> ppr t <> ")")
  FFst v -> (11, "(" <> ppr (valueToTerm v) <> "," <+> inner <> ")")
  FArg t _ -> (10, pparens (p < 10) inner <+> prettyPrec 11 t)
  FVArg v -> (10, pparens (p < 10) inner <+> prettyPrec 11 (valueToTerm v))
  FApp v -> (10, prettyPrec 10 (valueToTerm v) <+> pparens (p < 11) inner)
  FLet x _ t _ -> (11, hsep ["let", ppr x, "=", inner, "in", ppr t])
  FTry v -> (10, "try" <+> pparens (p < 11) inner <+> prettyPrec 11 (valueToTerm v))
  FExec -> prettyPrefix "E·" (p, inner)
  FBind Nothing _ t _ -> (0, pparens (p < 1) inner <+> ";" <+> ppr t)
  FBind (Just x) _ t _ -> (0, hsep [ppr x, "<-", pparens (p < 1) inner, ";", ppr t])
  FImmediate c _worldUpds _robotUpds -> prettyPrefix ("I[" <> ppr c <> "]·") (p, inner)
  FUpdate {} -> (p, inner)
  FFinishAtomic -> prettyPrefix "A·" (p, inner)
  FRcd _ done foc rest -> (11, encloseSep "[" "]" ", " (pDone ++ [pFoc] ++ pRest))
   where
    pDone = map (\(x, v) -> ppr x <+> "=" <+> ppr (valueToTerm v)) (reverse done)
    pFoc = ppr foc <+> "=" <+> inner
    pRest = map pprEq rest
    pprEq (x, Nothing) = ppr x
    pprEq (x, Just t) = ppr x <+> "=" <+> ppr t
  FProj x -> (11, pparens (p < 11) inner <> "." <> ppr x)
  FSuspend _ -> (10, "suspend" <+> pparens (p < 11) inner)
  FRestoreEnv _ -> (10, "restore" <+> pparens (p < 11) inner)

-- | Pretty-print a special "prefix application" frame, i.e. a frame
--   formatted like @X· inner@.  Unlike typical applications, these
--   associate to the *right*, so that we can print something like @X·
--   Y· Z· inner@ with no parens.
prettyPrefix :: Doc ann -> (Int, Doc ann) -> (Int, Doc ann)
prettyPrefix pre (p, inner) = (11, pre <+> pparens (p < 11) inner)

--------------------------------------------------------------
-- Runtime robot update
--------------------------------------------------------------

-- | Enumeration of robot updates.  This type is used for changes by
--   /e.g./ the @drill@ command which must be carried out at a later
--   tick. Using a first-order representation (as opposed to /e.g./
--   just a @Robot -> Robot@ function) allows us to serialize and
--   inspect the updates.
--
--   Note that this can not be in 'Swarm.Game.Robot' as it would create
--   a cyclic dependency.
data RobotUpdate
  = -- | Add copies of an entity to the robot's inventory.
    AddEntity Count Entity
  | -- | Make the robot learn about an entity.
    LearnEntity Entity
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RobotUpdate where
  toJSON = genericToJSON optionsMinimize

instance FromJSON RobotUpdate where
  parseJSON = genericParseJSON optionsMinimize
