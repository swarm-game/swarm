module Swarm.TUI.Editor.EditorView where

import Brick hiding (Direction)
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.List qualified as L
import Swarm.Game.World qualified as W
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Model
import Swarm.TUI.Panel

import Swarm.TUI.View.ViewUtils

drawWorldEditor :: FocusRing Name -> UIState -> Widget Name
drawWorldEditor toplevelFocusRing uis =
  if worldEditor ^. isWorldEditorEnabled
    then
      panel
        highlightAttr
        toplevelFocusRing
        (FocusablePanel WorldEditorPanel)
        plainBorder
        innerWidget
    else emptyWidget
 where
  privateFocusRing = worldEditor ^. editorFocusRing
  maybeCurrentFocus = focusGetCurrent privateFocusRing

  innerWidget =
    padLeftRight 1 $
      hLimit 30 $
        vBox
          [ brushWidget
          , -- , entityWidget
            areaWidget
          , outputWidget
          ]

  worldEditor = uis ^. uiWorldEditor
  maybeSelectedTerrain = fmap snd $ BL.listSelectedElement $ worldEditor ^. terrainList
  maybeAreaBounds = worldEditor ^. editingBounds

  -- TODO: Use withFocusRing
  mkFormControl n w =
    clickable n $ transformation w
   where
    transformation =
      if Just n == maybeCurrentFocus
        then withAttr BL.listSelectedFocusedAttr
        else id

  brushWidget =
    mkFormControl (WorldEditorPanelControl BrushSelector) $
      padRight (Pad 1) (str "Brush:") <+> brushWidgetContent

  brushWidgetContent =
    maybe emptyWidget drawLabeledTerrainSwatch maybeSelectedTerrain

  -- entityWidget =
  --   mkFormControl (WorldEditorPanelControl EntitySelector) $
  --     padRight (Pad 1) (str "Entity:") <+> entityWidgetContent

  -- entityWidgetContent =
  --   maybe emptyWidget drawLabeledTerrainSwatch maybeSelectedTerrain

  areaContent = case worldEditor ^. boundsSelectionStep of
    UpperLeftPending -> str "Click top-left"
    LowerRightPending _wcoords -> str "Click bottom-right"
    SelectionComplete -> maybe emptyWidget renderBounds maybeAreaBounds

  areaWidget =
    mkFormControl (WorldEditorPanelControl AreaSelector) $
      vBox
        [ str "Area:"
        , areaContent
        ]

  renderBounds (W.Coords primaryCorner@(x1, y1), W.Coords (x2, y2)) =
    str $ L.intercalate " @ " [rectSize, show primaryCorner]
   where
    width = x2 - x1
    -- NOTE: The height coordinate is inverted so we do opposite subtraction order here:
    height = y1 - y2
    rectSize = L.intercalate "x" [show width, show height]

  outputWidget =
    mkFormControl (WorldEditorPanelControl OutputPathSelector) $
      padRight (Pad 1) (str "Output:") <+> outputWidgetContent

  outputWidgetContent = str $ worldEditor ^. outputFilePath
