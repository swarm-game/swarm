{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A subrecord containing state specific to deciding when and which
-- parts of the world view to redraw.
module Swarm.Game.State.Redraw (
  Redraw,

  -- * Lenses
  dirtyCells,
  prevFocusRange,
  redrawWorldFlag,
  drawFrame,

  -- * Utility
  initRedraw,
  resetRedraw,
  needsRedraw,
  updateFocusRange,
  markDirtyCell,
  redrawWorld,
) where

import Control.Lens (Getter, Lens')
import Data.Set (Set)
import Data.Set qualified as S
import Swarm.Game.Location (Location)
import Swarm.Game.State.Range
import Swarm.Game.Universe (Cosmic)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | Subrecord of state relating to whether (and which parts of) the
--   world view needs to redrawn.
data Redraw = Redraw
  { _dirtyCells :: Set (Cosmic Location)
  , _prevFocusRange :: Maybe RobotRange
  , _redrawWorldFlag :: Bool
  , _drawFrame :: Bool
  }

makeLensesNoSigs ''Redraw

-- | Which locations have changed since the previous frame?  We will
--   need to be sure to redraw the view chunks containing these
--   locations.
dirtyCells :: Getter Redraw (Set (Cosmic Location))

-- | The range between the base and focused robot from the previous
--   frame.  Used to determine when we need to redraw the world since
--   the amount of static has changed.
prevFocusRange :: Getter Redraw (Maybe RobotRange)

-- | Should we redraw the entire world (i.e. invalidate the entire
--   view chunk cache) next frame?
redrawWorldFlag :: Getter Redraw Bool

-- | Should we make sure that we redraw the world pane, even if the
--   world view itself has not changed?  This might be relevant
--   e.g. for things displayed around the border of the world pane.
drawFrame :: Lens' Redraw Bool

-- | Initial 'Redraw' state, indicating that nothing needs to be
--   redrawn.
initRedraw :: Redraw
initRedraw = Redraw S.empty Nothing False False

-- | Reset the redraw state.  Typically this should be called right
--   after drawing each frame, to be ready to collect redrawing
--   information for the next frame.
--
--   Resets dirty cells and redraw flags, but preserves the previous
--   focus range (since the whole point is to remember what it was to
--   compare to the focus range next frame).
resetRedraw :: Redraw -> Redraw
resetRedraw r = initRedraw {_prevFocusRange = _prevFocusRange r}

-- | Does the world pane need redrawing at all?
needsRedraw :: Redraw -> Bool
needsRedraw (Redraw d _ w f) = w || f || not (S.null d)

-- | Update the current focus range.  If it has changed, mark the
--   world for redrawing.
updateFocusRange :: Maybe RobotRange -> Redraw -> Redraw
updateFocusRange newR r
  | _prevFocusRange r /= newR = redrawWorld $ r {_prevFocusRange = newR}
  | otherwise = r

-- | Mark a cell dirty, but if the entire world is already flagged for
--   a redraw, don't bother.
markDirtyCell :: Cosmic Location -> Redraw -> Redraw
markDirtyCell loc (Redraw d r w f) = Redraw ((if w then id else S.insert loc) d) r w f

-- | Mark the entire world for a redraw.  Clear out the set of dirty
--   locations since it doesn't matter anymore.
redrawWorld :: Redraw -> Redraw
redrawWorld (Redraw _ r _ f) = Redraw S.empty r True f
