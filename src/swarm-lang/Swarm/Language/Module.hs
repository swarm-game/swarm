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
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Import qualified as Import
import Swarm.Language.Syntax.Util (Erasable (..))
import Swarm.Language.Types (TCtx, TDCtx, UCtx)

-- | The context for a module, containing names and types of things
--   defined in the module (once typechecking has run).
type family ModuleCtx (phase :: Phase) where
  ModuleCtx Raw = ()
  ModuleCtx Resolved = ()
  ModuleCtx Inferred = (UCtx, TDCtx)
  ModuleCtx Typed = (TCtx, TDCtx)
  ModuleCtx Elaborated = (TCtx, TDCtx)
  ModuleCtx Instantiated = (TCtx, TDCtx)

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
  -- ^ The context of names defined in this module and their types.
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
