{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Display logic for Structures.
module Swarm.TUI.View.Structure (
  renderStructuresDisplay,
  makeListWidget,
) where

import Brick hiding (Direction, Location, getName)
import Brick.Focus
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List qualified as BL
import Control.Lens hiding (Const, from)
import Data.Map.NonEmpty qualified as NEM
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Swarm.Game.Entity (Entity, entityDisplay)
import Swarm.Game.Scenario (StructureCells)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Placement (getStructureName)
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition (foundStructures, recognitionState)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (getEntityGrid)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (foundByName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.State.Substate (structureRecognition)
import Swarm.Language.Syntax.Direction (directionJsonModifier)
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Structure
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.CellDisplay
import Swarm.TUI.View.Util
import Swarm.Util (commaList)

-- | Render a two-pane widget with structure selection on the left
-- and single-structure details on the right.
structureWidget :: GameState -> StructureInfo StructureCells Entity -> Widget n
structureWidget gs s =
  vBox
    [ hBox
        [ headerItem "Name" theName
        , padLeft (Pad 2)
            . headerItem "Size"
            . T.pack
            . renderRectDimensions
            . getAreaDimensions
            $ entityProcessedGrid s
        , occurrenceCountSuffix
        ]
    , reorientabilityWidget
    , maybeDescriptionWidget
    , padTop (Pad 1) $
        hBox
          [ structureIllustration
          , padLeft (Pad 4) ingredientsBox
          ]
    ]
 where
  headerItem h content =
    hBox
      [ padRight (Pad 1) $ txt $ h <> ":"
      , withAttr boldAttr $ txt content
      ]

  annotatedStructureGrid = annotatedGrid s

  supportedOrientations = Set.toList . Structure.recognize . namedGrid $ annotatedStructureGrid

  renderSymmetry = \case
    NoSymmetry -> "no"
    TwoFold -> "2-fold"
    FourFold -> "4-fold"

  reorientabilityWidget =
    txt $
      T.unwords
        [ "Orientable:"
        , commaList $ map (T.pack . directionJsonModifier . show) supportedOrientations
        , "with"
        , renderSymmetry $ symmetry annotatedStructureGrid
        , "rotational symmetry."
        ]

  maybeDescriptionWidget =
    maybe emptyWidget (padTop (Pad 1) . withAttr italicAttr . txtWrap) $
      Structure.description . namedGrid . annotatedGrid $
        s

  registry = gs ^. discovery . structureRecognition . recognitionState . foundStructures
  occurrenceCountSuffix = case M.lookup theName $ foundByName registry of
    Nothing -> emptyWidget
    Just inner -> padLeft (Pad 2) . headerItem "Count" . T.pack . show $ NEM.size inner

  structureIllustration = vBox $ map (hBox . map renderOneCell) cells
  d = namedGrid $ annotatedGrid s

  ingredientsBox =
    vBox
      [ padBottom (Pad 1) $ withAttr boldAttr $ txt "Materials:"
      , ingredientLines
      ]
  ingredientLines = vBox . map showCount . M.toList $ entityCounts s

  showCount (e, c) =
    hBox
      [ drawLabelledEntityName e
      , txt $
          T.unwords
            [ ":"
            , T.pack $ show c
            ]
      ]

  theName = getStructureName $ Structure.name d
  cells = getEntityGrid d
  renderOneCell = maybe (txt " ") (renderDisplay . view entityDisplay)

makeListWidget :: [StructureInfo StructureCells Entity] -> BL.List Name (StructureInfo StructureCells Entity)
makeListWidget structureDefinitions =
  BL.listMoveTo 0 $ BL.list (StructureWidgets StructuresList) (V.fromList structureDefinitions) 1

renderStructuresDisplay :: GameState -> StructureDisplay -> Widget Name
renderStructuresDisplay gs structureDisplay =
  vBox
    [ hBox
        [ leftSide
        , padLeft (Pad 2) structureElaboration
        ]
    , footer
    ]
 where
  footer = hCenter $ withAttr italicAttr $ txt "NOTE: [Tab] toggles focus between panes"
  lw = _structurePanelListWidget structureDisplay
  fr = _structurePanelFocus structureDisplay
  leftSide =
    hLimitPercent 25 $
      padAll 1 $
        vBox
          [ hCenter $ withAttr boldAttr $ txt "Candidates"
          , padAll 1 $
              vLimit 10 $
                withFocusRing fr (BL.renderList drawSidebarListItem) lw
          ]

  -- Adds very subtle coloring to indicate focus switch
  highlightIfFocused = case focusGetCurrent fr of
    Just (StructureWidgets StructureSummary) -> withAttr lightCyanAttr
    _ -> id

  -- Note: An extra "padRight" is inserted to account for the vertical scrollbar,
  -- whether or not it appears.
  structureElaboration =
    clickable (StructureWidgets StructureSummary)
      . maybeScroll ModalViewport
      . maybe emptyWidget (padAll 1 . padRight (Pad 1) . highlightIfFocused . structureWidget gs . snd)
      $ BL.listSelectedElement lw

drawSidebarListItem ::
  Bool ->
  StructureInfo StructureCells Entity ->
  Widget Name
drawSidebarListItem _isSelected (StructureInfo annotated _ _) =
  txt . getStructureName . Structure.name $ namedGrid annotated
