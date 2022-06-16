{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Swarm.Language.Capability
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Capabilities needed to evaluate and execute programs.  If you're
-- curious about how this works and/or thinking about creating some
-- additional capabilities, you're encouraged to read the extensive
-- comments in the source code.
module Swarm.Language.Capability (
  Capability (..),
  CapCtx,
  capabilityName,
  requiredCaps,
  constCaps,
) where

import Data.Char (toLower)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Witch (from)
import Prelude hiding (lookup)

import Data.Data (Data)
import Data.Yaml
import GHC.Generics (Generic)

import Swarm.Language.Context
import Swarm.Language.Syntax

-- | Various capabilities which robots can have.
data Capability
  = -- | Execute the 'Move' command
    CMove
  | -- | Execute the 'Turn' command
    --
    -- NOTE: using cardinal directions is separate 'COrient' capability
    CTurn
  | -- | Execute the 'Selfdestruct' command
    CSelfdestruct
  | -- | Execute the 'Grab' command
    CGrab
  | -- | Execute the 'Place' command
    CPlace
  | -- | Execute the 'Give' command
    CGive
  | -- | Execute the 'Install' command
    CInstall
  | -- | Execute the 'Make' command
    CMake
  | -- | Execute the 'Count' command
    CCount
  | -- | Execute the 'Build' command
    CBuild
  | -- | Execute the 'Salvage' command
    CSalvage
  | -- | Execute the 'Drill' command
    CDrill
  | -- | Execute the 'Whereami' command
    CSenseloc
  | -- | Execute the 'Blocked' command
    CSensefront
  | -- | Execute the 'Ishere' command
    CSensehere
  | -- | Execute the 'Scan' command
    CScan
  | -- | Execute the 'Random' command
    CRandom
  | -- | Execute the 'Appear' command
    CAppear
  | -- | Execute the 'Create' command
    CCreate
  | -- | Execute the 'Log' command
    CLog
  | -- | Don't drown in liquid
    CFloat
  | -- | Evaluate conditional expressions
    CCond
  | -- | Evaluate comparison operations
    CCompare
  | -- | Use cardinal direction constants.
    COrient
  | -- | Evaluate arithmetic operations
    CArith
  | -- | Store and look up definitions in an environment
    CEnv
  | -- | Interpret lambda abstractions
    CLambda
  | -- | Enable recursive definitions
    CRecursion
  | -- | Execute the 'Reprogram' command
    CReprogram
  | -- | Capability to introspect and see its own name
    CWhoami
  | -- | Capability to set its own name
    CSetname
  | -- | God-like capabilities.  For e.g. commands intended only for
    --   checking challenge mode win conditions, and not for use by
    --   players.
    CGod
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable, Data)

capabilityName :: Capability -> Text
capabilityName = from @String . map toLower . drop 1 . show

instance ToJSON Capability where
  toJSON = String . capabilityName

instance FromJSON Capability where
  parseJSON = withText "Capability" tryRead
   where
    tryRead :: Text -> Parser Capability
    tryRead t = case readMaybe . from . T.cons 'C' . T.toTitle $ t of
      Just c -> return c
      Nothing -> fail $ "Unknown capability " ++ from t

-- | A capability context records the capabilities required by the
--   definitions bound to variables.
type CapCtx = Ctx (Set Capability)

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
requiredCaps :: CapCtx -> Term -> (Set Capability, CapCtx)
requiredCaps ctx tm = case tm of
  -- First, at the top level, we have to keep track of the
  -- capabilities needed by variables bound with the 'TDef' command.

  -- To make a definition requires the env capability.  Note that the
  -- act of MAKING the definition does not require the capabilities of
  -- the body of the definition (including the possibility of the
  -- recursion capability, if the definition is recursive).  However,
  -- we also return a map which associates the defined name to the
  -- capabilities it requires.
  TDef r x _ t ->
    let bodyCaps = (if r then S.insert CRecursion else id) (requiredCaps' ctx t)
     in (S.singleton CEnv, singleton x bodyCaps)
  TBind _ t1 t2 ->
    -- First, see what capabilities are required to execute the
    -- first command.  It may also define some names, so we get a
    -- map of those names to their required capabilities.
    let (caps1, ctx1) = requiredCaps ctx t1

        -- Now see what capabilities are required for the second
        -- command; use an extended context since it may refer to
        -- things defined in the first command.
        ctx' = ctx `union` ctx1
        (caps2, ctx2) = requiredCaps ctx' t2
     in -- Finally return the union of everything.
        (caps1 `S.union` caps2, ctx' `union` ctx2)
  -- Any other term can't bind variables with 'TDef', so we no longer
  -- need to worry about tracking a returned context.
  _ -> (requiredCaps' ctx tm, empty)

-- | Infer the capabilities required to execute/evaluate a term in a
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
requiredCaps' :: CapCtx -> Term -> Set Capability
requiredCaps' = go
 where
  go ctx tm = case tm of
    -- Some primitive literals that don't require any special
    -- capability.
    TUnit -> S.empty
    TDir d -> if isCardinal d then S.singleton COrient else S.empty
    TInt _ -> S.empty
    TAntiInt _ -> S.empty
    TString _ -> S.empty
    TAntiString _ -> S.empty
    TBool _ -> S.empty
    -- Look up the capabilities required by a function/command
    -- constants using 'constCaps'.
    TConst c -> constCaps c
    -- Note that a variable might not show up in the context, and
    -- that's OK.  In particular, only variables bound by 'TDef' go
    -- in the context; variables bound by a lambda or let will not
    -- be there.
    TVar x -> fromMaybe S.empty (lookup x ctx)
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
    TLam x _ t -> S.insert CLambda $ go (delete x ctx) t
    -- An application simply requires the union of the capabilities
    -- from the left- and right-hand sides.  This assumes that the
    -- argument will be used at least once by the function.
    TApp t1 t2 -> go ctx t1 `S.union` go ctx t2
    -- Similarly, for a let, we assume that the let-bound expression
    -- will be used at least once in the body. We delete the let-bound
    -- name from the context when recursing for the same reason as
    -- lambda.
    TLet r x _ t1 t2 ->
      (if r then S.insert CRecursion else id) $
        S.insert CEnv $ go (delete x ctx) t1 `S.union` go (delete x ctx) t2
    -- We also delete the name in a TBind, if any, while recursing on
    -- the RHS.
    TBind mx t1 t2 -> go ctx t1 `S.union` go (maybe id delete mx ctx) t2
    -- Everything else is straightforward.
    TPair t1 t2 -> go ctx t1 `S.union` go ctx t2
    TDelay _ t -> go ctx t
    -- This case should never happen if the term has been
    -- typechecked; Def commands are only allowed at the top level,
    -- so simply returning S.empty is safe.
    TDef {} -> S.empty

-- | Capabilities needed to evaluate or execute a constant.
constCaps :: Const -> Set Capability
constCaps =
  S.fromList . \case
    -- Some built-in constants that don't require any special capability.
    Wait -> []
    Noop -> []
    AppF -> []
    Force -> []
    Return -> []
    Self -> []
    Parent -> []
    Base -> []
    Setname -> []
    Undefined -> []
    ErrorStr -> []
    -- Some straightforward ones.
    Log -> [CLog]
    Selfdestruct -> [CSelfdestruct]
    Move -> [CMove]
    Turn -> [CTurn]
    Grab -> [CGrab]
    Place -> [CPlace]
    Give -> [CGive]
    Install -> [CInstall]
    Make -> [CMake]
    Has -> []
    Count -> [CCount]
    If -> [CCond]
    Create -> [CCreate]
    Blocked -> [CSensefront]
    Scan -> [CScan]
    Ishere -> [CSensehere]
    Upload -> [CScan]
    Build -> [CBuild]
    Salvage -> [CSalvage]
    Reprogram -> [CReprogram]
    Drill -> [CDrill]
    Neg -> [CArith]
    Add -> [CArith]
    Sub -> [CArith]
    Mul -> [CArith]
    Div -> [CArith]
    Exp -> [CArith]
    -- Some God-like abilities.
    Teleport -> [CGod]
    As -> [CGod]
    RobotNamed -> [CGod]
    -- String operations, which for now are enabled by CLog
    Format -> [CLog]
    Concat -> [CLog]
    -- Some additional straightforward ones, which however currently
    -- cannot be used in classic mode since there is no craftable item
    -- which conveys their capability.
    Appear -> [CAppear] -- paint?
    Whereami -> [CSenseloc] -- GPS?
    Random -> [CRandom] -- randomness device (with bitcoins)?
    Whoami -> [CWhoami] -- mirror, needs a recipe

    -- comparator?
    Eq -> [CCompare]
    Neq -> [CCompare]
    Lt -> [CCompare]
    Gt -> [CCompare]
    Leq -> [CCompare]
    Geq -> [CCompare]
    And -> []
    Or -> []
    -- Some more constants which *ought* to have their own capability but
    -- currently don't.
    Say -> []
    View -> [] -- XXX this should also require something.
    Run -> [] -- XXX this should also require a capability
    -- which the base starts out with.
    Not -> [] -- XXX some kind of boolean logic cap?
    Inl -> [] -- XXX should require cap for sums
    Inr -> []
    Case -> []
    Fst -> [] -- XXX should require cap for pairs
    Snd -> []
    Try -> [] -- XXX these definitely need to require
    Raise -> [] -- something.
