{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer
module Swarm.Game.Scenario.Topography.Structure.Recognition where

import Swarm.Game.Scenario.Topography.Structure

data StructureRecognizer = StructureRecognizer {
    definitions :: InheritedStructureDefs
  , foo :: Int
  }