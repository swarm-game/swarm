module Swarm.TUI.Model.Name where

data WorldEditorFocusable
  = BrushSelector
  | EntitySelector
  | AreaSelector
  | OutputPathSelector
  | MapSaveButton
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

-- | 'Name' represents names to uniquely identify various components
--   of the UI, such as forms, panels, caches, extents, and lists.
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
  | -- | The list of scenario choices.
    ScenarioList
  | -- | The scrollable viewport for the info panel.
    InfoViewport
  | -- | The scrollable viewport for any modal dialog.
    ModalViewport
  deriving (Eq, Ord, Show, Read)
