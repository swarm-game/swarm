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
import Data.List.NonEmpty qualified as NE
import Data.Map.Ordered qualified as OM
import Data.Text (Text)
import Data.Vector qualified as V
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Entity as E
import Swarm.Game.Ingredients
import Swarm.Game.ScenarioInfo (
  ScenarioCollection,
  ScenarioInfo (..),
  ScenarioInfoPair,
  ScenarioItem (..),
  scMap,
  scenarioCollectionToList,
 )
import Swarm.Game.World.Gen (Seed)
import Swarm.TUI.Model.Name
import System.FilePath (dropTrailingPathSeparator, splitPath, takeFileName)
import Witch (into)

------------------------------------------------------------
-- Menus and dialogs
------------------------------------------------------------

data ScenarioOutcome = WinModal | LoseModal
  deriving (Show, Eq)

data ModalType
  = HelpModal
  | RecipesModal
  | CommandsModal
  | MessagesModal
  | StructuresModal
  | EntityPaletteModal
  | TerrainPaletteModal
  | RobotsModal
  | ScenarioEndModal ScenarioOutcome
  | QuitModal
  | KeepPlayingModal
  | DescriptionModal Entity
  | GoalModal
  deriving (Show, Eq)

data ButtonAction
  = Cancel
  | KeepPlaying
  | StartOver Seed (ScenarioInfoPair ScenarioInfo)
  | QuitAction
  | Next (ScenarioInfoPair ScenarioInfo)

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
    NewGameMenu (NonEmpty (BL.List Name (ScenarioItem ScenarioInfo)))
  | AchievementsMenu (BL.List Name CategorizedAchievement)
  | MessagesMenu
  | AboutMenu

mainMenu :: MainMenuEntry -> BL.List Name MainMenuEntry
mainMenu e = BL.list MenuList (V.fromList enumerate) 1 & BL.listMoveToElement e

makePrisms ''Menu

-- | Create a brick 'BL.List' of scenario items from a 'ScenarioCollection'.
mkScenarioList :: ScenarioCollection ScenarioInfo -> BL.List Name (ScenarioItem ScenarioInfo)
mkScenarioList = flip (BL.list ScenarioList) 1 . V.fromList . scenarioCollectionToList

-- | Given a 'ScenarioCollection' and a 'FilePath' which is the canonical
--   path to some folder or scenario, construct a 'NewGameMenu' stack
--   focused on the given item, if possible.
mkNewGameMenu :: ScenarioCollection ScenarioInfo -> FilePath -> Maybe Menu
mkNewGameMenu sc path = fmap NewGameMenu $ NE.nonEmpty =<< go (Just sc) (splitPath path) []
 where
  go ::
    Maybe (ScenarioCollection ScenarioInfo) ->
    [FilePath] ->
    [BL.List Name (ScenarioItem ScenarioInfo)] ->
    Maybe [BL.List Name (ScenarioItem ScenarioInfo)]
  go _ [] stk = Just stk
  go Nothing _ _ = Nothing
  go (Just curSC) (thing : rest) stk = go nextSC rest (lst : stk)
   where
    hasName :: ScenarioItem ScenarioInfo -> Bool
    hasName (SISingle (_, ScenarioInfo pth _)) = takeFileName pth == thing
    hasName (SICollection nm _) = nm == into @Text (dropTrailingPathSeparator thing)

    lst = BL.listFindBy hasName (mkScenarioList curSC)

    nextSC = case OM.lookup (dropTrailingPathSeparator thing) (scMap curSC) of
      Just (SICollection _ c) -> Just c
      _ -> Nothing

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
