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

{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Entities where

import           Control.Lens
import           Data.Bool
import           Data.List                 (foldl')
import           Prelude                   hiding (log)

import           Swarm.Game.Display
import           Swarm.Game.Entity
import           Swarm.Language.Capability
import           Swarm.TUI.Attr

------------------------------------------------------------
-- All entities
------------------------------------------------------------

entityCatalog :: Inventory
entityCatalog = foldl' (flip insert) empty
  [ tree, branch, log, wood
  , pebbles, rock, mountain
  , flower, lambda
  , wave, bit False, bit True
  , treads, grabber

  , flerb, elephant
  ]

------------------------------------------------------------
-- Basic entities
------------------------------------------------------------

tree :: Entity
tree = mkEntity
  (defaultEntityDisplay 'T' & displayAttr .~ plantAttr)
  "tree"
  ["A tree."]
  [Portable, Growable]

branch :: Entity
branch = mkEntity
  (defaultEntityDisplay 'y' & displayAttr .~ woodAttr)
  "branch"
  ["A branch from a tree."]
  [Portable]

log :: Entity
log = mkEntity
  (defaultEntityDisplay 'l' & displayAttr .~ woodAttr)
  "log"
  ["A wooden log, obtained by harvesting a tree."]
  [Portable]

wood :: Entity
wood = mkEntity
  (defaultEntityDisplay 'w' & displayAttr .~ woodAttr)
  "wood"
  ["A wooden plank."]
  [Portable]

rock :: Entity
rock = mkEntity
  (defaultEntityDisplay '@' & displayAttr .~ rockAttr)
  "rock"
  ["A rock."]
  [Unwalkable]

mountain :: Entity
mountain = mkEntity
  (defaultEntityDisplay '∧' & displayAttr .~ snowAttr)
  "mountain"
  ["A mountain."]
  [Unwalkable]

pebbles :: Entity
pebbles = mkEntity
  (defaultEntityDisplay ':')
  "pebbles"
  ["Some pebbles."]
  [Portable]

flower :: Entity
flower = mkEntity
  (defaultEntityDisplay '*' & displayAttr .~ flowerAttr)
  "flower"
  ["A flower."]
  [Portable, Growable]

lambda :: Entity
lambda = mkEntity
  (defaultEntityDisplay 'λ' & displayAttr .~ flowerAttr)
  "lambda"
  ["A lambda."]
  [Portable]

wave :: Entity
wave = mkEntity
  (defaultEntityDisplay '~' & displayAttr .~ waterAttr)
  "wave"
  ["A wave on the surface of the water."]
  []

bit :: Bool -> Entity
bit b = mkEntity
  (defaultEntityDisplay (bool '0' '1' b))
  (bool "zero bit" "one bit" b)
  ["A bit."]
  [Portable]

linux :: Entity
linux = mkEntity
  (defaultEntityDisplay 'L')
  "Linux"
  ["A copy of the Linux operating system."]
  [Portable]

flerb :: Entity
flerb = mkEntity
  (defaultEntityDisplay '&')
  "flerb"
  ["It is a flerb.", "What is a flerb, you ask? No one knows."]
  [Portable, Growable]

elephant :: Entity
elephant = mkEntity
  (defaultEntityDisplay 'E' & displayAttr .~ rockAttr)
  "elephant"
  ["An elephant.", "It is very big."]
  [Unwalkable]

------------------------------------------------------------
-- Devices
------------------------------------------------------------

treads :: Entity
treads = mkDevice '%'
  "treads"
  [ "Installing treads on a robot allows it to move (via the 'move' command) and turn (via the 'turn' command)."
  , "Example: {move; turn left; move; turn north}"
  ]
  [CMove, CTurn]

grabber :: Entity
grabber = mkDevice '<'
  "grabber"
  [ "A grabber arm allows a robot to grab an item on its current cell, via the 'grab' command."
  , "Example: {grab \"tree\"}"
  ]
  [CGrab]

solarPanels :: Entity
solarPanels = mkDevice '#'
  "solar panel"
  ["Solar panels provide power for a robot indefinitely."]
  []

-- entitiesByCapability :: Map Capability [Entity]
-- entitiesByCapability = undefined

-- XXX 3D printer device you need for Build

-- XXX Ability to read description text when highlighting items in inventory.
