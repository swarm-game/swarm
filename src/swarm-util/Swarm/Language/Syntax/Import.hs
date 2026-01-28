{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types to represent Swarm-lang import locations.
module Swarm.Language.Syntax.Import (
  -- * Import phase
  ImportPhase (..),

  -- * Anchors
  Anchor,
  RAnchor (..),
  UAnchor (..),
  pattern Root,
  pattern Drive,
  pattern Web,
  pattern Local,
  pattern Home,
  pattern Swarm,

  -- * ImportDir
  ImportDir,
  mkImportDir,
  withImportDir,

  -- ** Pre-defined dirs
  homeDir,
  currentDir,

  -- * ImportLoc
  ImportLoc (..),
  importAnchor,

  -- ** Utilities
  Path (..),
  anchorToPath,
  dirToPath,
  locToPath,
  locToFilePath,
  (<//>),
) where

import Swarm.Language.Syntax.Import.Internal
