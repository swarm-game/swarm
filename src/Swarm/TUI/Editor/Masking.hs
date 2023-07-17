module Swarm.TUI.Editor.Masking where

import Control.Lens hiding (Const, from)
import Data.Maybe (fromMaybe)
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model.UI

shouldHideWorldCell :: UIState -> W.Coords -> Bool
shouldHideWorldCell ui coords =
  isOutsideSingleSelectedCorner || isOutsideMapSaveBounds
 where
  we = ui ^. uiWorldEditor
  withinTimeout = ui ^. lastFrameTime < we ^. editingBounds . boundsPersistDisplayUntil

  isOutsideMapSaveBounds =
    withinTimeout
      && fromMaybe
        False
        ( do
            bounds <- we ^. editingBounds . boundsRect
            pure $ EU.isOutsideRegion (bounds ^. planar) coords
        )

  isOutsideSingleSelectedCorner = fromMaybe False $ do
    Cosmo _ cornerCoords <- case we ^. editingBounds . boundsSelectionStep of
      LowerRightPending cornerCoords -> Just cornerCoords
      _ -> Nothing
    pure $ EU.isOutsideTopLeftCorner cornerCoords coords
