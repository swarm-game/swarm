{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Constants used throughout the UI and game.
module Swarm.Constant where

import Data.Text (Text)

-- * Website constants

-- $convention
-- By convention, all URL constants include trailing slashes
-- when applicable.

-- | The URL for the Swarm repository.
swarmRepoUrl :: Text
swarmRepoUrl = "https://github.com/swarm-game/swarm/"

-- | The URL for the Swarm wiki.
wikiUrl :: Text
wikiUrl = swarmRepoUrl <> "wiki/"

-- | The URL for the Swarm commands cheat sheet.
wikiCheatSheet :: Text
wikiCheatSheet = wikiUrl <> "Commands-Cheat-Sheet"
