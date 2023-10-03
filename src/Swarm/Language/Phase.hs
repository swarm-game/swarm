-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Language pipeline phase identifiers.
module Swarm.Language.Phase where

-- | The possible phases of a Swarm language term in the pipeline,
--   listed in order from beginning to end.
data Phase
  = -- | The term has been successfully parsed into an AST, but any
    --   imports are still unresolved.
    Parsed
  | -- | All imports have been statically resolved: /i.e./ any
    -- imported terms have been loaded from the disk or network, and
    -- those in turn have been parsed and recursively resolved.
    Resolved
  | -- | We are in the middle of type inference; the term has been
    --   annotated with UTypes, i.e. types still containing
    --   unification variables.
    Inferred
  | -- | The term has been successfully typechecked and annotated.
    Checked
  | -- | The term has been successfully elaborated.
    Elaborated
