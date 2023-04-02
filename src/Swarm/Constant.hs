{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Constants used throughout the UI and game
module Swarm.Constant where

import Data.Text (Text)

-- * Website constants

-- By convention, all URL constants include trailing slashes
-- when applicable.

swarmRepoUrl :: Text
swarmRepoUrl = "https://github.com/swarm-game/swarm/"

wikiUrl :: Text
wikiUrl = swarmRepoUrl <> "wiki/"

wikiCheatSheet :: Text
wikiCheatSheet = wikiUrl <> "Commands-Cheat-Sheet"
