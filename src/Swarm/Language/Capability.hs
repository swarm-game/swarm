{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Capability
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Capabilities needed to evaluate and execute programs.
--
-----------------------------------------------------------------------------

module Swarm.Language.Capability
  ( Capability(..), CapCtx
  , requiredCaps
  , constCaps
  ) where

import           Data.Hashable         (Hashable)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Set.Lens         (setOf)
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Data.Yaml
import           GHC.Generics          (Generic)

import           Data.Char             (toLower)
import           Data.Maybe            (fromMaybe)
import           Swarm.Language.Syntax
import           Text.Read             (readMaybe)
import           Witch                 (from)


-- | Various capabilities which robots can have.
data Capability
  = CMove      -- ^ Execute the 'Move' command
  | CTurn      -- ^ Execute the 'Turn' command
  | CGrab      -- ^ Execute the 'Grab' command
  | CPlace     -- ^ Execute the 'Place' command
  | CGive      -- ^ Execute the 'Give' command
  | CMake      -- ^ Execute the 'Make' command
  | CBuild     -- ^ Execute the 'Build' command
  | CSenseloc  -- ^ Execute the 'GetX' and 'GetY' commands
  | CSensefront -- ^ Execute the 'Blocked' command
  | CRandom    -- ^ Execute the 'Random' command
  | CAppear    -- ^ Execute the 'Appear' command

  | CCond      -- ^ Evaluate conditional expressions
  | CCompare   -- ^ Evaluate comparison operations
  | CArith     -- ^ Evaluate arithmetic operations
  | CEnv       -- ^ Store and look up definitions in an environment
  | CLambda    -- ^ Interpret lambda abstractions

  | CRecursion -- ^ Enable recursive definitions
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

instance ToJSON Capability where
  toJSON = String . from . map toLower . drop 1 . show

instance FromJSON Capability where
  parseJSON = withText "Capability" tryRead
    where
      tryRead :: Text -> Parser Capability
      tryRead t = case readMaybe . from . T.cons 'C' . T.toTitle $ t of
        Just c  -> return c
        Nothing -> fail $ "Unknown capability " ++ from t

type CapCtx = Map Var (Set Capability)

-- | Analyze a program to see what capabilities may be needed to
--   execute it. Also returns a mapping from any variables (declared
--   via 'TDef') to the capabilities needed by their definitions.
--
--   Note that this is necessarily a conservative analysis, especially
--   if the program contains conditional expressions.  Some
--   capabilities may end up not being actually needed if certain
--   commands end up not being executed.  However, the analysis is
--   safe in the sense that a robot with the indicated capabilities
--   will always be able to run the given program.
requiredCaps :: CapCtx -> Term -> (Set Capability, CapCtx)
requiredCaps ctx tm = case tm of

  -- First, at the top level, we have to keep track of the
  -- capabilities needed by variables bound with the 'TDef' command.

  -- To make a definition requires the env capability.  Note that the
  -- act of MAKING the definition does not require the capabilities of
  -- the body of the definition.  However, we also return a map
  -- which associates the defined name to the capabilities it requires.
  TDef x _ t ->
    let bodyCaps = (if x `S.member` setOf fv t then S.insert CRecursion else id) (requiredCaps' ctx t)
    in (S.singleton CEnv, M.singleton x bodyCaps)
    -- Note that you can MAKE a recursive definition with only CEnv,
    -- but RUNNING it requires CRecursion.

  TBind _ t1 t2 ->

        -- First, see what capabilities are required to execute the
        -- first command.  It may also define some names, so we get a
        -- map of those names to their required capabilities.
    let (caps1, ctx1) = requiredCaps ctx t1

        -- Now see what capabilities are required for the second
        -- command; use an extended context since it may refer to
        -- things defined in the first command.
        ctx'          = M.union ctx1 ctx
        (caps2, ctx2) = requiredCaps ctx' t2

        -- Finally return the union of everything.
    in  (caps1 `S.union` caps2, M.union ctx2 ctx')

  -- Any other term can't bind variables with 'TDef', so we no longer
  -- need to worry about tracking a returned context.
  _ -> (requiredCaps' ctx tm, M.empty)

-- | Infer the capabilities required to execute/evaluate a term in a
--   given context, where the term is guaranteed not to have 'TDef'.
--
--   For function application and let-expressions, we assume that the
--   argument (respectively let-bound expression) is used at least
--   once in the body.  Doing otherwise would require a much more
--   fine-grained analysis where we differentiate between the
--   capabilities needed to *evaluate* versus *execute* any expression
--   (since e.g. an unused let-binding would still incur the
--   capabilities to *evaluate* it), which does not seem worth it at
--   all.
requiredCaps' :: CapCtx -> Term -> Set Capability
requiredCaps' ctx = go
  where
    go tm = case tm of
      TUnit          -> S.empty
      TConst c       -> constCaps c
      TDir _         -> S.empty
      TInt _         -> S.empty
      TString _      -> S.empty
      TBool _        -> S.empty

      -- Note that a variable might not show up in the context, and
      -- that's OK.  In particular, only variables bound by 'TDef' go
      -- in the context; variables bound by a lambda or let will not
      -- be there.
      TVar x         -> fromMaybe S.empty (M.lookup x ctx)

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
      TLam _ _ t     -> S.insert CLambda $ go t

      -- An application simply requires the union of the capabilities
      -- from the left- and right-hand sides.  This assumes that the
      -- argument will be used at least once by the function.
      TApp t1 t2     -> go t1 `S.union` go t2

      -- Similarly, for a let, we assume that the let-bound expression
      -- will be used at least once in the body.
      TLet x _ t1 t2 ->
        (if x `S.member` setOf fv t1 then S.insert CRecursion else id)
        $ S.insert CEnv $ go t1 `S.union` go t2

      -- Everything else is straightforward.
      TPair t1 t2    -> go t1 `S.union` go t2
      TBind _ t1 t2  -> go t1 `S.union` go t2
      TDelay t       -> go t

      -- This case should never happen if the term has been
      -- typechecked; Def commands are only allowed at the top level,
      -- so simply returning S.empty is safe.
      TDef{}         -> S.empty

-- | Capabilities needed to evaluate/execute a constant.
constCaps :: Const -> Set Capability
constCaps Wait         = S.empty
constCaps Noop         = S.empty
constCaps Selfdestruct = S.empty

constCaps Move         = S.singleton CMove
constCaps Turn         = S.singleton CTurn
constCaps Grab         = S.singleton CGrab
constCaps Place        = S.singleton CPlace
constCaps Give         = S.singleton CGive
constCaps Make         = S.singleton CMake
constCaps Build        = S.singleton CBuild
  -- XXX need to do something more sophisticated for Build?

-- It's important that no capability is required for 'say', because
-- this is how exceptions get reported.
constCaps Say          = S.empty

constCaps View         = S.empty
constCaps Appear       = S.singleton CAppear

constCaps GetX         = S.singleton CSenseloc
constCaps GetY         = S.singleton CSenseloc
constCaps Blocked      = S.singleton CSensefront
constCaps Ishere       = S.empty
constCaps Random       = S.singleton CRandom

constCaps Run          = S.empty

constCaps Not          = S.empty
constCaps (Cmp _)      = S.singleton CCompare
constCaps Neg          = S.singleton CArith
constCaps (Arith _)    = S.singleton CArith

constCaps If           = S.singleton CCond
constCaps Fst          = S.empty
constCaps Snd          = S.empty
constCaps Force        = S.empty
constCaps Return       = S.empty
constCaps Try          = S.empty
constCaps Raise        = S.empty
