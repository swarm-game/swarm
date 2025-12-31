{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A module is an AST along with various associated metadata, such as
-- a context, set of imports, timestamp, and where the module was
-- loaded from.
module Swarm.Language.Module where

import Data.Aeson (ToJSON)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Strict.Tuple
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Import qualified as Import
import Swarm.Language.Syntax.Util (Erasable (..))
import Swarm.Language.Types (TCtx, TDCtx, UCtx)

-- | The context for a module, which allows us to quickly answer the
--   question: if we import this module, what things are brought into
--   scope?  In particular:
--
--   - Before the module is typechecked (i.e. in either the Raw or
--     Resolved phase) we do not yet have this information.
--
--   - During the Inferred phase, we have a 'UCtx' containing inferred
--     types for any variables defined with @def@, and a 'TDCtx'
--     containing the definitions of any type aliases defined with
--     @tydef@.
--
--   - During all subsequent phases, the 'UCtx' becomes a 'TCtx'
--     containing fully generalized/quantified types.
--
--  See also Note [Module exports].
type family ModuleCtx (phase :: Phase) where
  ModuleCtx Raw = ()
  ModuleCtx Resolved = ()
  ModuleCtx Inferred = Pair UCtx TDCtx
  ModuleCtx Typed = Pair TCtx TDCtx
  ModuleCtx Elaborated = Pair TCtx TDCtx
  ModuleCtx Instantiated = Pair TCtx TDCtx

-- | With a newly parsed, raw module we don't know any import
--   information.  After that, we cache the set of imports for a
--   module to enable easy import cycle checks.
type family ModuleImports (phase :: Phase) where
  ModuleImports Raw = ()
  ModuleImports _ = Set (ImportLoc Import.Resolved)

-- | Where did a module come from?  This can be used in printing
--   better error messages, and is also used to determine how to
--   interpret relative imports.  For example, if module A imports
--   "../B.sw" then we look at A's provenance and go up one directory
--   from there to look for B.
data ModuleProvenance where
  -- | The module was loaded directly from a particular file path.
  --   This will be the case e.g. for a .sw file specified on the
  --   command line, or for code loaded from a world description .yaml
  --   file.
  FromFile :: FilePath -> ModuleProvenance
  -- | The module was loaded as the result of an import directive; we
  --   record its fully resolved and canonicalized location.
  FromImport :: ImportLoc Import.Resolved -> ModuleProvenance
  -- | We have no provenance information for the module.  This is
  --   typically because it is code that was entered at the REPL, but
  --   could also be because e.g. it was parsed from code embedded in
  --   Markdown, or because it is a placeholder of some sort.
  NoProvenance :: ModuleProvenance
  deriving (Show, Eq, Data, Generic, Hashable, ToJSON)

-- | A 'Module' is a (possibly empty) AST, along with a context for
--   any definitions contained in it, and a list of transitive,
--   canonicalized imports.
data Module phase = Module
  { moduleTerm :: Maybe (Syntax phase)
  -- ^ The contents of the module.
  , moduleCtx :: ModuleCtx phase
  -- ^ The context of names exported by this module: variables defined
  --   via @def@ with their types, and type aliases defined via @tydef@
  --   with their definitions.  See Note [Module exports].
  , moduleImports :: ModuleImports phase
  -- ^ The moduleImports are mostly for convenience, e.g. for checking modules for cycles.
  , moduleTimestamp :: Maybe UTCTime
  -- ^ The time at which the module was loaded
  , moduleProvenance :: ModuleProvenance
  -- ^ Where the module came from
  }
  deriving (Generic)

deriving instance (Show (Anchor (ImportPhaseFor phase)), Show (SwarmType phase), Show (ModuleCtx phase), Show (ModuleImports phase)) => Show (Module phase)
deriving instance (Eq (ModuleImports phase), Eq (ModuleCtx phase), Eq (SwarmType phase), Eq (Anchor (ImportPhaseFor phase))) => Eq (Module phase)
deriving instance (Eq (Anchor (ImportPhaseFor phase)), Data (Anchor (ImportPhaseFor phase)), Typeable phase, Typeable (ImportPhaseFor phase), Data (ModuleCtx phase), Data (ModuleImports phase), Data (SwarmType phase)) => Data (Module phase)
deriving instance (Hashable (ModuleImports phase), Hashable (ModuleCtx phase), Hashable (SwarmType phase), Hashable (Anchor (ImportPhaseFor phase)), Generic (Anchor (ImportPhaseFor phase))) => Hashable (Module phase)

emptyModule :: Module Elaborated
emptyModule = Module Nothing mempty S.empty Nothing NoProvenance

instance Erasable Module where
  erase (Module t _ _ time prov) = Module (erase <$> t) () S.empty time prov
  eraseRaw (Module t _ _ time prov) = Module (eraseRaw <$> t) () () time prov

-- ~~~~ Note [Module exports]
--
-- In line with the way e.g. Haskell modules work, and to aid in
-- modularity/encapsulation, if module A imports module B, then the
-- exports of module B are in scope in module A, but module A does NOT
-- (by default) re-export things imported from B.  By default, module
-- A exports *only* names which are explicitly defined in A.
--
-- For example, the following should be accepted:
--
-- A.sw:
-- def a = 1 end
--
-- B.sw:
-- import "A"
-- def b = a + 1 end
--
-- C.sw:
-- import "B"
-- def c = b + 1 end
--
-- However, if the definition of c were changed to `def c = b + a end`
-- then it would result in an error, since `a` is not in scope in C:
-- `a` is exported from A.sw and imported into B.sw (and can hence be
-- used in the definition of `b`), but not re-exported from B.sw.
-- Hence `a` is not in scope in C.sw, only `b`.  (Of course, we could
-- add `import "A"` to C.sw in which case `def c = a + b` would work
-- just fine.)
--
-- The `moduleCtx` field of a Module record records the names that are
-- exported by the module, along with their types (for value-level
-- names) or their definitions (for type synonyms).  Note that the
-- 'ModuleCtx' type family defines what this field should hold at
-- various stages of the processing pipeline. The field is populated
-- by 'Swarm.Language.Typecheck.collectDefs', which is called from
-- 'Swarm.Language.Typecheck.inferModule'.  'collectDefs' recurses
-- through the AST and simply finds all top-level definitions, while
-- ignoring any imports.
--
-- When typechecking a module containing imports, we simply look up
-- the 'moduleCtx' of each and add them to the typechecking context.
--
-- At runtime, we cache evaluated environments for modules so that we
-- do not have to re-evaluate modules every time we see an import.
-- However, at the end of evaluating a module, the ambient environment
-- contains both variables defined in the module as well as anything
-- in scope from its imports.  Hence we use
-- 'Swarm.Language.Value.restrictEnv' to restrict the cached
-- environment to only those names which are supposed to be exported.
