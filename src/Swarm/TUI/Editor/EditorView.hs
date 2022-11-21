module Swarm.TUI.Editor.EditorView where

import Brick hiding (Direction)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (getEditContents, renderEditor)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.Table qualified as BT
import Control.Lens hiding (Const, from)
import Control.Monad (guard)
import Control.Monad.Reader (withReaderT)
import Data.Array (range)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable qualified as F
import Data.Functor (($>))
import Data.IntMap qualified as IM
import Data.List (intersperse)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Graphics.Vty qualified as V
import Linear
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.CESK (CESK (..))
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Scenario (scenarioAuthor, scenarioDescription, scenarioName, scenarioObjectives)
import Swarm.Game.ScenarioInfo (
  ScenarioItem (..),
  ScenarioStatus (..),
  scenarioBestTicks,
  scenarioBestTime,
  scenarioItemName,
  scenarioStatus,
 )
import Swarm.Game.State
import Swarm.Game.Terrain (TerrainType, terrainMap)
import Swarm.Game.World qualified as W
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types (Polytype)
import Swarm.TUI.Attr
import Swarm.TUI.Border
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Inventory.Sorting (renderSortMethod)
import Swarm.TUI.Model
import Swarm.TUI.Panel
import Swarm.Util
import Swarm.Version (NewReleaseFailure (..))
import System.Clock (TimeSpec (..))
import Text.Printf
import Text.Wrap
import Witch (from, into)

import Swarm.TUI.View.ViewUtils

drawWorldEditor :: FocusRing Name -> UIState -> Widget Name
drawWorldEditor toplevelFocusRing uis =
  if worldEditor ^. isWorldEditorEnabled
    then
      panel
        highlightAttr
        toplevelFocusRing
        WorldEditorPanel
        ( plainBorder
            -- TODO FIXME
            & topLabels . rightLabel .~ (drawType <$> (uis ^. uiREPL . replType))
        )
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
