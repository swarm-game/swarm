{-# LANGUAGE TypeData #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Phases in the processing pipeline for language terms + robot records.
module Swarm.Language.Phase (
  Phase (..)
) where

-- | XXX
type data Phase where
  -- | Raw, parsed terms that have not yet been type checked.
  Raw :: Phase
  -- | Imports have been resolved to canonical locations.
  Resolved :: Phase
  -- | Terms have inferred types.  XXX unification variables
  Inferred :: Phase
  -- | Terms have been typechecked, but robot records
  --   are still "templates", not yet instantiated to concrete robots.
  Typed :: Phase
  -- | Robot records instantiated to concrete robots.
  Instantiated :: Phase

-- XXX note: no separate Elaborated phase, having elaboration be
-- type-changing would be annoying.
