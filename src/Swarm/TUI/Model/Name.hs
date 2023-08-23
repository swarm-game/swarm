-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Sum types representing the Brick names
-- for every referenceable widget.
--
-- Nesting of name types is utilized often to simplify
-- case matching.
module Swarm.TUI.Model.Name where

data WorldEditorFocusable
  = BrushSelector
  | EntitySelector
  | AreaSelector
  | OutputPathSelector
  | MapSaveButton
  | ClearEntityButton
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data FocusablePanel
  = -- | The panel containing the REPL.
    REPLPanel
  | -- | The panel containing the world view.
    WorldPanel
  | -- | The panel containing the world editor controls.
    WorldEditorPanel
  | -- | The panel showing robot info and inventory on the top left.
    RobotPanel
  | -- | The info panel on the bottom left.
    InfoPanel
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ScenarioConfigPanel
  = ScenarioConfigFileSelector
  | ScenarioConfigPanelControl ScenarioConfigPanelFocusable
  deriving (Eq, Ord, Show, Read)

data ScenarioConfigPanelFocusable
  = 
    AutoPlaySelector
  | -- | The file selector for launching a scenario with a script
    ScriptSelector
  | SeedSelector
  | StartGameButton
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GoalWidget
  = ObjectivesList
  | GoalSummary
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Clickable buttons in modal dialogs.
data Button
  = CancelButton
  | KeepPlayingButton
  | StartOverButton
  | QuitButton
  | NextButton
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | 'Name' represents names to uniquely identify various components
--   of the UI, such as forms, panels, caches, extents, lists, and buttons.
data Name
  = FocusablePanel FocusablePanel
  | -- | An individual control within the world editor panel.
    WorldEditorPanelControl WorldEditorFocusable
  | -- | The REPL input form.
    REPLInput
  | -- | The render cache for the world view.
    WorldCache
  | -- | The cached extent for the world view.
    WorldExtent
  | -- | The cursor/viewCenter display in the bottom left of the World view
    WorldPositionIndicator
  | -- | The list of possible entities to paint a map with.
    EntityPaintList
  | -- | The entity paint item position in the EntityPaintList.
    EntityPaintListItem Int
  | -- | The list of possible terrain materials.
    TerrainList
  | -- | The terrain item position in the TerrainList.
    TerrainListItem Int
  | -- | The list of inventory items for the currently
    --   focused robot.
    InventoryList
  | -- | The inventory item position in the InventoryList.
    InventoryListItem Int
  | -- | The list of main menu choices.
    MenuList
  | -- | The list of achievements.
    AchievementList
  | -- | An individual control within the scenario launch config panel
    ScenarioConfigControl ScenarioConfigPanel
  | -- | The list of goals/objectives.
    GoalWidgets GoalWidget
  | -- | The list of scenario choices.
    ScenarioList
  | -- | The scrollable viewport for the info panel.
    InfoViewport
  | -- | The scrollable viewport for any modal dialog.
    ModalViewport
  | -- | A clickable button in a modal dialog.
    Button Button
  deriving (Eq, Ord, Show, Read)
