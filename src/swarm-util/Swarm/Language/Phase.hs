{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Phases in the processing pipeline for language terms + robot records.
module Swarm.Language.Phase (
  Phase (..),
  ImportPhaseFor,
) where

import Swarm.Language.Syntax.Import qualified as Import

-- | Phases of the Swarm language processing pipeline.
type data Phase where
  -- | Raw, parsed terms that have not yet been type checked.
  Raw :: Phase
  -- | Imports have been resolved to canonical locations; imports have
  --   been recursively loaded and there are no import cycles.
  Resolved :: Phase
  -- | Terms have inferred types, containing unification variables.
  Inferred :: Phase
  -- | Inferred types have been properly quantified.  Terms have been
  --   typechecked, but robot records are still "templates", not yet
  --   instantiated to concrete robots.
  Typed :: Phase
  -- | Robot records have been instantiated to concrete robots.
  Instantiated :: Phase

-- ~~~~ Note [Elaborated phase]
--
-- In some ways, it would make a lot of sense to have a separate
-- Elaborated phase between Typed and Instantiated.  In particular
-- this would help ensure that we cannot forget to run elaboration.
-- However, we don't do this for an annoying technical reason: we use
-- a generic traversal to implement elaboration which must have a type
-- like `a -> a`, so we can't have it  XXX... use unsafeCoerce?

-- XXX
type family ImportPhaseFor (p :: Phase) :: Import.ImportPhase where
  ImportPhaseFor Raw = Import.Raw
  ImportPhaseFor p = Import.Resolved
