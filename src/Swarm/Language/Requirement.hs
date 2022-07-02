-- |
-- Module      :  Swarm.Language.Requirement
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A requirement is something that is needed in order to successfully
-- build a robot running a certain program.
module Swarm.Language.Requirement (
  -- * Requirements

  -- ** The 'Requirement' type
  Requirement (..),

  -- ** The 'Requirements' type and utility functions
  Requirements (..),
  singleton,
  singletonCap,
  singletonDev,
  singletonInv,
  insert,
  ReqCtx,

  -- * Requirements analysis
  requirements,
) where

import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import GHC.Generics (Generic)

import Swarm.Language.Capability (Capability (..), constCaps)
import Swarm.Language.Context (Ctx)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Syntax

-- | A /requirement/ is something a robot must have when it is
--   built. There are three types:
--   - A robot can require a certain 'Capability', which should be fulfilled
--     by installing an appropriate device.
--   - A robot can require a specific /device/, which should be installed.
--   - A robot can require some number of a specific entity in its inventory.
data Requirement
  = -- | Require a specific capability.  This must be fulfilled by
    --   installing an appropriate device.  Requiring the same
    --   capability multiple times is the same as requiring it once.
    ReqCap Capability
  | -- | Require a specific device to be installed.  Note that at this
    --   point it is only a name, and has not been resolved to an actual
    --   'Entity'.  That's because programs have to be type- and
    --   capability-checked independent of an 'EntityMap'.  The name
    --   will be looked up at runtime, when actually executing a 'Build'
    --   or 'Reprogram' command, and an appropriate exception thrown if
    --   a device with the given name does not exist.
    --
    --   Requiring the same device multiple times is the same as
    --   requiring it once.
    ReqDev Text
  | -- | Require a certain number of a specific entity to be available
    --   in the inventory.  The same comments apply re: resolving the
    --   entity name to an actual 'Entity'.
    --
    --   Inventory requirements are additive, that is, say, requiring 5
    --   of entity `e` and later requiring 7 is the same as requiring
    --   12.
    ReqInv Integer Text
  deriving (Eq, Ord, Show, Read, Generic, Hashable, Data)

-- | It is tempting to define @Requirements = Set Requirement@, but
--   that would be wrong, since two identical 'ReqInv' should have
--   their counts added rather than simply being deduplicated.
--
--   Since we will eventually need to deal with the different types of
--   requirements separately, it makes sense to store them separately
--   anyway.
data Requirements = Requirements
  { capReqs :: Set Capability
  , devReqs :: Set Text
  , invReqs :: Map Text Integer
  }
  deriving (Eq, Ord, Show, Data)

instance Semigroup Requirements where
  Requirements c1 d1 i1 <> Requirements c2 d2 i2 =
    Requirements (c1 <> c2) (d1 <> d2) (M.unionWith (+) i1 i2)

instance Monoid Requirements where
  mempty = Requirements S.empty S.empty M.empty

-- | Create a 'Requirements' set with a single 'Requirement'.
singleton :: Requirement -> Requirements
singleton (ReqCap c) = Requirements (S.singleton c) S.empty M.empty
singleton (ReqDev d) = Requirements S.empty (S.singleton d) M.empty
singleton (ReqInv n e) = Requirements S.empty S.empty (M.singleton e n)

-- | For convenience, create a 'Requirements' set with a single
--   'Capability' requirement.
singletonCap :: Capability -> Requirements
singletonCap = singleton . ReqCap

-- | For convenience, create a 'Requirements' set with a single
--   device requirement.
singletonDev :: Text -> Requirements
singletonDev = singleton . ReqDev

-- | For convenience, create a 'Requirements' set with a single
--   inventory requirement.
singletonInv :: Integer -> Text -> Requirements
singletonInv n e = singleton (ReqInv n e)

insert :: Requirement -> Requirements -> Requirements
insert = (<>) . singleton

-- | A requirement context records the requirements for the
--   definitions bound to variables.
type ReqCtx = Ctx Requirements

-- | Analyze a program to see what capabilities may be needed to
--   execute it. Also return a capability context mapping from any
--   variables declared via 'TDef' to the capabilities needed by
--   their definitions.
--
--   Note that this is necessarily a conservative analysis, especially
--   if the program contains conditional expressions.  Some
--   capabilities may end up not being actually needed if certain
--   commands end up not being executed.  However, the analysis should
--   be safe in the sense that a robot with the indicated capabilities
--   will always be able to run the given program.
requirements :: ReqCtx -> Term -> (Requirements, ReqCtx)
requirements ctx tm = first (insert (ReqCap CPower)) $ case tm of
  -- First, at the top level, we have to keep track of the
  -- requirements for variables bound with the 'TDef' command.

  -- To make a definition requires the env capability.  Note that the
  -- act of MAKING the definition does not require the capabilities of
  -- the body of the definition (including the possibility of the
  -- recursion capability, if the definition is recursive).  However,
  -- we also return a map which associates the defined name to the
  -- capabilities it requires.
  TDef r x _ t ->
    let bodyReqs = (if r then insert (ReqCap CRecursion) else id) (requirements' ctx t)
     in (singletonCap CEnv, Ctx.singleton x bodyReqs)
  TBind _ t1 t2 ->
    -- First, see what capabilities are required to execute the
    -- first command.  It may also define some names, so we get a
    -- map of those names to their required capabilities.
    let (caps1, ctx1) = requirements ctx t1

        -- Now see what capabilities are required for the second
        -- command; use an extended context since it may refer to
        -- things defined in the first command.
        ctx' = ctx `Ctx.union` ctx1
        (caps2, ctx2) = requirements ctx' t2
     in -- Finally return the union of everything.
        (caps1 <> caps2, ctx' `Ctx.union` ctx2)
  -- Any other term can't bind variables with 'TDef', so we no longer
  -- need to worry about tracking a returned context.
  _ -> (requirements' ctx tm, Ctx.empty)

-- | Infer the requirements to execute/evaluate a term in a
--   given context, where the term is guaranteed not to contain any
--   'TDef'.
--
--   For function application and let-expressions, we assume that the
--   argument (respectively let-bound expression) is used at least
--   once in the body.  Doing otherwise would require a much more
--   fine-grained analysis where we differentiate between the
--   capabilities needed to *evaluate* versus *execute* any expression
--   (since e.g. an unused let-binding would still incur the
--   capabilities to *evaluate* it), which does not seem worth it at
--   all.
requirements' :: ReqCtx -> Term -> Requirements
requirements' = go
 where
  go ctx tm = case tm of
    -- Some primitive literals that don't require any special
    -- capability.
    TUnit -> mempty
    TDir d -> if isCardinal d then singletonCap COrient else mempty
    TInt _ -> mempty
    TAntiInt _ -> mempty
    TString _ -> mempty
    TAntiString _ -> mempty
    TBool _ -> mempty
    -- Look up the capabilities required by a function/command
    -- constants using 'constCaps'.
    TConst c -> maybe mempty singletonCap (constCaps c)
    -- Simply record device or inventory requirements.
    TRequireDevice d -> singletonDev d
    TRequire n e -> singletonInv n e
    -- Note that a variable might not show up in the context, and
    -- that's OK.  In particular, only variables bound by 'TDef' go
    -- in the context; variables bound by a lambda or let will not
    -- be there.
    TVar x -> fromMaybe mempty (Ctx.lookup x ctx)
    -- A lambda expression requires the 'CLambda' capability, and
    -- also all the capabilities of the body.  We assume that the
    -- lambda will eventually get applied, at which point it will
    -- indeed require the body's capabilities (this is unnecessarily
    -- conservative if the lambda is never applied, but such a
    -- program could easily be rewritten without the unused
    -- lambda). We also don't do anything with the argument: we
    -- assume that it is used at least once within the body, and the
    -- capabilities required by any argument will be picked up at
    -- the application site.  Again, this is overly conservative in
    -- the case that the argument is unused, but in that case the
    -- unused argument could be removed.
    --
    -- Note, however, that we do need to *delete* the argument from
    -- the context, in case the context already contains a definition
    -- with the same name: inside the lambda that definition will be
    -- shadowed, so we do not want the name to be associated to any
    -- capabilities.
    TLam x _ t -> insert (ReqCap CLambda) $ go (Ctx.delete x ctx) t
    -- An application simply requires the union of the capabilities
    -- from the left- and right-hand sides.  This assumes that the
    -- argument will be used at least once by the function.
    TApp t1 t2 -> go ctx t1 <> go ctx t2
    -- Similarly, for a let, we assume that the let-bound expression
    -- will be used at least once in the body. We delete the let-bound
    -- name from the context when recursing for the same reason as
    -- lambda.
    TLet r x _ t1 t2 ->
      (if r then insert (ReqCap CRecursion) else id) $
        insert (ReqCap CEnv) $ go (Ctx.delete x ctx) t1 <> go (Ctx.delete x ctx) t2
    -- We also delete the name in a TBind, if any, while recursing on
    -- the RHS.
    TBind mx t1 t2 -> go ctx t1 <> go (maybe id Ctx.delete mx ctx) t2
    -- Everything else is straightforward.
    TPair t1 t2 -> go ctx t1 <> go ctx t2
    TDelay _ t -> go ctx t
    -- This case should never happen if the term has been
    -- typechecked; Def commands are only allowed at the top level,
    -- so simply returning mempty is safe.
    TDef {} -> mempty
