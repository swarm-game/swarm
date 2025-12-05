{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A module is an AST along with
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

-- | A module only needs to record its imports during resolution, so
--   we can do cyclic import detection.  After that we no longer
--   require the information.
type family ModuleImports (phase :: Phase) where
  ModuleImports Raw = ()
  ModuleImports Resolved = Set (ImportLoc Import.Resolved)
  ModuleImports Inferred = ()
  ModuleImports Typed = ()
  ModuleImports Elaborated = ()
  ModuleImports Instantiated = ()

data ModuleProvenance where
  FromFile :: FilePath -> ModuleProvenance
  FromImport :: ImportLoc Import.Resolved -> ModuleProvenance
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
emptyModule = Module Nothing mempty () Nothing NoProvenance

instance Erasable Module where
  erase (Module t _ _ time prov) = Module (erase <$> t) () S.empty time prov
  eraseRaw (Module t _ _ time prov) = Module (eraseRaw <$> t) () () time prov
