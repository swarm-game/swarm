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
  -- | All inferred types have been properly quantified.
  Typed :: Phase
  -- | Typechecked terms have been typechecked and elaborated.
  --   However, robot records are still "templates", i.e. not yet
  --   instantiated to concrete robots.
  --
  --   Invariant: all type families taking 'Phase' as an argument must
  --   return equal results on 'Typed' and 'Elaborated'; elaboration
  --   is implemented via a type-fixing generic transformation
  --   followed by 'unsafeCoerce'.
  Elaborated :: Phase
  -- | Robot records have been instantiated to concrete robots.
  Instantiated :: Phase

-- | Map each 'Phase' to the corresponding
--   'Swarm.Language.Syntax.ImportPhase'.  In particular 'Raw' maps to
--   'Swarm.Language.Syntax.Raw', and everything else maps to
--   'Swarm.Language.Syntax.Resolved'.
type family ImportPhaseFor (p :: Phase) :: Import.ImportPhase where
  ImportPhaseFor Raw = Import.Raw
  ImportPhaseFor p = Import.Resolved
