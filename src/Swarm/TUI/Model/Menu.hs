{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Swarm.TUI.Model.Menu where

import Brick.Widgets.Dialog (Dialog)
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (from, (<.>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text (Text)
import Data.Vector qualified as V
import Swarm.Game.Entity as E
import Swarm.Game.ScenarioInfo (
  ScenarioCollection,
  ScenarioInfo (..),
  ScenarioInfoPair,
  ScenarioItem (..),
  scMap,
  scenarioCollectionToList,
 )
import Swarm.Game.State
import Swarm.TUI.Model.Achievement.Definitions
import Swarm.TUI.Model.Name
import Swarm.Util
import System.FilePath (dropTrailingPathSeparator, splitPath, takeFileName)
import Witch (into)

------------------------------------------------------------
-- Menus and dialogs
------------------------------------------------------------

data ModalType
  = HelpModal
  | RecipesModal
  | CommandsModal
  | MessagesModal
  | RobotsModal
  | WinModal
  | LoseModal
  | QuitModal
  | KeepPlayingModal
  | DescriptionModal Entity
  | GoalModal
  deriving (Show)

data ButtonAction
  = Cancel
  | KeepPlaying
  | StartOver Seed ScenarioInfoPair
  | QuitAction
  | Next ScenarioInfoPair

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
  = NoMenu -- We started playing directly from command line, no menu to show
  | MainMenu (BL.List Name MainMenuEntry)
  | -- Stack of scenario item lists. INVARIANT: the currently selected
    -- menu item is ALWAYS the same as the scenario currently being played.
    -- See https://github.com/swarm-game/swarm/issues/1064 and
    -- https://github.com/swarm-game/swarm/pull/1065.
    NewGameMenu (NonEmpty (BL.List Name ScenarioItem))
  | AchievementsMenu (BL.List Name CategorizedAchievement)
  | MessagesMenu
  | AboutMenu

mainMenu :: MainMenuEntry -> BL.List Name MainMenuEntry
mainMenu e = BL.list MenuList (V.fromList listEnums) 1 & BL.listMoveToElement e

makePrisms ''Menu

-- | Create a brick 'BL.List' of scenario items from a 'ScenarioCollection'.
mkScenarioList :: Bool -> ScenarioCollection -> BL.List Name ScenarioItem
mkScenarioList cheat = flip (BL.list ScenarioList) 1 . V.fromList . filterTest . scenarioCollectionToList
 where
  filterTest = if cheat then id else filter (\case SICollection n _ -> n /= "Testing"; _ -> True)

-- | Given a 'ScenarioCollection' and a 'FilePath' which is the canonical
--   path to some folder or scenario, construct a 'NewGameMenu' stack
--   focused on the given item, if possible.
mkNewGameMenu :: Bool -> ScenarioCollection -> FilePath -> Maybe Menu
mkNewGameMenu cheat sc path = NewGameMenu . NE.fromList <$> go (Just sc) (splitPath path) []
 where
  go :: Maybe ScenarioCollection -> [FilePath] -> [BL.List Name ScenarioItem] -> Maybe [BL.List Name ScenarioItem]
  go _ [] stk = Just stk
  go Nothing _ _ = Nothing
  go (Just curSC) (thing : rest) stk = go nextSC rest (lst : stk)
   where
    hasName :: ScenarioItem -> Bool
    hasName (SISingle (_, ScenarioInfo pth _ _ _)) = takeFileName pth == thing
    hasName (SICollection nm _) = nm == into @Text (dropTrailingPathSeparator thing)

    lst = BL.listFindBy hasName (mkScenarioList cheat curSC)

    nextSC = case M.lookup (dropTrailingPathSeparator thing) (scMap curSC) of
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
