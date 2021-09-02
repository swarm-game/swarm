{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Entities
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some common entities.
--
-----------------------------------------------------------------------------

module Swarm.Game.Entities where

import           Control.Lens

import           Swarm.Game.Display
import           Swarm.Game.Entity
import           Swarm.TUI.Attr

------------------------------------------------------------
-- Basic entities
------------------------------------------------------------

tree :: Entity
tree = mkEntity
  (defaultEntityDisplay 'T' & displayAttr .~ plantAttr)
  "Tree"
  "It is a tree."
  Nothing
  [Portable, Growable]

rock :: Entity
rock = mkEntity
  (defaultEntityDisplay '@' & displayAttr .~ rockAttr)
  "Rock"
  "A rock."
  Nothing
  [Unwalkable]

------------------------------------------------------------
-- Devices
------------------------------------------------------------

treads :: Entity
treads = mkEntity
  (defaultEntityDisplay '%' & displayAttr .~ deviceAttr)
  "Treads"
  "Installing treads on a robot allows it to move (via the 'move' command) and turn (via the 'turn' command)."
  Nothing
  [Portable]
