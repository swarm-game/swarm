{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Sum types that represent menu options,
-- modal dialogs, and buttons.
module Swarm.TUI.Model.Menu where

import Brick.Widgets.Dialog (Dialog)
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (<.>))
import Data.List.Extra (enumerate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Vector qualified as V
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Entity as E
import Swarm.Game.Ingredients
import Swarm.Game.Scenario.Status (ScenarioPath (..))
import Swarm.Game.ScenarioInfo (
  ScenarioCollection,
  ScenarioItem (..),
  ScenarioWith,
  scenarioCollectionToList,
 )
import Swarm.Game.World.Gen (Seed)
import Swarm.TUI.Model.Name

------------------------------------------------------------
-- Menus and dialogs
------------------------------------------------------------

data ScenarioOutcome = WinModal | LoseModal
  deriving (Show, Eq)

data MidScenarioModalType
  = HelpModal
  | RecipesModal
  | CommandsModal
  | MessagesModal
  | StructuresModal
  | EntityPaletteModal
  | TerrainPaletteModal
  | RobotsModal
  | DescriptionModal Entity
  | GoalModal
  deriving (Show, Eq)

data EndScenarioModalType
  = ScenarioFinishModal ScenarioOutcome
  | QuitModal
  | KeepPlayingModal
  deriving (Show, Eq)

data ModalType
  = MidScenarioModal MidScenarioModalType
  | EndScenarioModal EndScenarioModalType
  deriving (Show, Eq)

data ButtonAction
  = Cancel
  | KeepPlaying
  | StartOver Seed (ScenarioWith ScenarioPath)
  | QuitAction
  | Next (ScenarioWith ScenarioPath)

data Modal = Modal
  { _modalType :: ModalType
  , _modalDialog :: Dialog ButtonAction Name
  }

makeLenses ''Modal

data MainMenuEntry
  = NewGame
  | Tutorial
  | Achievements
  | Messages
  | About
  | Quit
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Menu
  = -- | We started playing directly from command line, no menu to show
    NoMenu
  | MainMenu (BL.List Name MainMenuEntry)
  | -- | Stack of scenario item lists. INVARIANT: the currently selected
    -- menu item is ALWAYS the same as the scenario currently being played.
    -- See https://github.com/swarm-game/swarm/issues/1064 and
    -- https://github.com/swarm-game/swarm/pull/1065.
    NewGameMenu (NonEmpty (BL.List Name (ScenarioItem ScenarioPath)))
  | AchievementsMenu (BL.List Name CategorizedAchievement)
  | MessagesMenu
  | AboutMenu

mainMenu :: MainMenuEntry -> BL.List Name MainMenuEntry
mainMenu e = BL.list MenuList (V.fromList enumerate) 1 & BL.listMoveToElement e

makePrisms ''Menu

-- | Create a brick 'BL.List' of scenario items from a 'ScenarioCollection'.
mkScenarioList :: ScenarioCollection a -> BL.List Name (ScenarioItem a)
mkScenarioList =
  flip (BL.list ScenarioList) 1
    . V.fromList
    . scenarioCollectionToList

------------------------------------------------------------
-- Inventory list entries
------------------------------------------------------------

-- | An entry in the inventory list displayed in the info panel.  We
--   can either have an entity with a count in the robot's inventory,
--   an entity equipped on the robot, or a labelled separator.  The
--   purpose of the separators is to show a clear distinction between
--   the robot's /inventory/ and its /equipped devices/.
data InventoryListEntry
  = Separator Text
  | InventoryEntry Count Entity
  | EquippedEntry Entity
  deriving (Eq)

makePrisms ''InventoryListEntry
