module Swarm.TUI.Editor.View where

import Brick hiding (Direction)
import Brick.Focus
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.List qualified as L
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.World qualified as W
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Editor.Area qualified as EA
import Swarm.TUI.Editor.Model
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import Swarm.TUI.Panel
import Swarm.TUI.View.CellDisplay (renderDisplay)
import Swarm.TUI.View.Util qualified as VU
import Swarm.Util (listEnums)

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

  controlsBox =
    padBottom Max $
      vBox
        [ brushWidget
        , entityWidget
        , clearEntityButtonWidget
        , areaWidget
        , outputWidget
        , str " "
        , saveButtonWidget
        ]

  innerWidget =
    padLeftRight 1 $
      hLimit 30 $
        controlsBox <=> statusBox

  worldEditor = uis ^. uiWorldEditor
  maybeAreaBounds = worldEditor ^. editingBounds . boundsRect

  -- TODO (#1150): Use withFocusRing?
  mkFormControl n w =
    clickable n $ transformation w
   where
    transformation =
      if Just n == maybeCurrentFocus
        then withAttr BL.listSelectedFocusedAttr
        else id

  swatchContent list drawFunc =
    maybe emptyWidget drawFunc selectedThing
   where
    selectedThing = snd <$> BL.listSelectedElement list

  brushWidget =
    mkFormControl (WorldEditorPanelControl BrushSelector) $
      padRight (Pad 1) (str "Brush:")
        <+> swatchContent (worldEditor ^. terrainList) VU.drawLabeledTerrainSwatch

  entityWidget =
    mkFormControl (WorldEditorPanelControl EntitySelector) $
      padRight (Pad 1) (str "Entity:")
        <+> swatchContent (worldEditor ^. entityPaintList) drawLabeledEntitySwatch

  clearEntityButtonWidget =
    if null (worldEditor ^. entityPaintList . BL.listSelectedL)
      then emptyWidget
      else
        mkFormControl (WorldEditorPanelControl ClearEntityButton)
          . hLimit 20
          . hCenter
          $ str "None"

  areaContent = case worldEditor ^. editingBounds . boundsSelectionStep of
    UpperLeftPending -> str "Click top-left"
    LowerRightPending _wcoords -> str "Click bottom-right"
    SelectionComplete -> maybe emptyWidget renderBounds maybeAreaBounds

  areaWidget =
    mkFormControl (WorldEditorPanelControl AreaSelector) $
      vBox
        [ str "Area:"
        , areaContent
        ]

  renderBounds (upperLeftCoord, lowerRightCoord) =
    str $
      unwords $
        L.intersperse
          "@"
          [ EA.renderRectDimensions rectArea
          , VU.locationToString upperLeftLoc
          ]
   where
    upperLeftLoc = W.coordsToLoc upperLeftCoord
    lowerRightLoc = W.coordsToLoc lowerRightCoord
    rectArea = EA.cornersToArea upperLeftLoc lowerRightLoc

  outputWidget =
    mkFormControl (WorldEditorPanelControl OutputPathSelector) $
      padRight (Pad 1) (str "Output:") <+> outputWidgetContent

  outputWidgetContent = str $ worldEditor ^. outputFilePath

  saveButtonWidget =
    mkFormControl (WorldEditorPanelControl MapSaveButton)
      . hLimit 20
      . hCenter
      $ str "Save"

  statusBox = maybe emptyWidget str $ worldEditor ^. lastWorldEditorMessage

drawLabeledEntitySwatch :: EntityFacade -> Widget Name
drawLabeledEntitySwatch (EntityFacade eName eDisplay) =
  tile <+> txt eName
 where
  tile = padRight (Pad 1) $ renderDisplay eDisplay

drawTerrainSelector :: AppState -> Widget Name
drawTerrainSelector s =
  padAll 1
    . hCenter
    . vLimit (length (listEnums :: [TerrainType]))
    . BL.renderListWithIndex listDrawTerrainElement True
    $ s ^. uiState . uiWorldEditor . terrainList

listDrawTerrainElement :: Int -> Bool -> TerrainType -> Widget Name
listDrawTerrainElement pos _isSelected a =
  clickable (TerrainListItem pos) $ VU.drawLabeledTerrainSwatch a

drawEntityPaintSelector :: AppState -> Widget Name
drawEntityPaintSelector s =
  padAll 1
    . hCenter
    . vLimit 10
    . BL.renderListWithIndex listDrawEntityPaintElement True
    $ s ^. uiState . uiWorldEditor . entityPaintList

listDrawEntityPaintElement :: Int -> Bool -> EntityFacade -> Widget Name
listDrawEntityPaintElement pos _isSelected a =
  clickable (EntityPaintListItem pos) $ drawLabeledEntitySwatch a
