module Swarm.TUI.Model.Name where

data FocusablePanel
  = -- | The panel containing the REPL.
    REPLPanel
  | -- | The panel containing the world view.
    WorldPanel
  | -- | The panel showing robot info and inventory on the top left.
    RobotPanel
  | -- | The info panel on the bottom left.
    InfoPanel
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GoalWidget
  = ObjectivesList
  | GoalSummary
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Clickable buttons in modal dilaogs.
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
  | -- | The REPL input form.
    REPLInput
  | -- | The render cache for the world view.
    WorldCache
  | -- | The cached extent for the world view.
    WorldExtent
  | -- | The list of inventory items for the currently
    --   focused robot.
    InventoryList
  | -- | The inventory item position in the InventoryList.
    InventoryListItem Int
  | -- | The list of main menu choices.
    MenuList
  | -- | The list of achievements.
    AchievementList
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
