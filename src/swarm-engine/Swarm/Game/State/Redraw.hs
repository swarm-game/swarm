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
  redrawWorld,
  drawFrame,

  -- * Utility
  initRedraw,
  resetRedraw,
  needsRedraw,
) where

import Control.Lens (Lens', (^.))
import Data.Set (Set)
import Data.Set qualified as S
import Swarm.Game.Location (Location)
import Swarm.Game.Universe (Cosmic)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | Subrecord of state relating to whether (and which parts of) the
--   world view needs to redrawn.
data Redraw = Redraw
  { _dirtyCells :: Set (Cosmic Location)
  , _redrawWorld :: Bool
  , _drawFrame :: Bool
  }

makeLensesNoSigs ''Redraw

-- | Which locations have changed since the previous frame?  We will
--   need to be sure to redraw the view chunks containing these
--   locations.
dirtyCells :: Lens' Redraw (Set (Cosmic Location))

-- | Should we redraw the entire world (i.e. invalidate the entire
--   view chunk cache) next frame?
redrawWorld :: Lens' Redraw Bool

-- | Should we make sure that we redraw the world pane, even if the
--   world view itself has not changed?  This might be relevant
--   e.g. for things displayed around the border of the world pane.
drawFrame :: Lens' Redraw Bool

-- | Initial 'Redraw' state, indicating that nothing needs to be
--   redrawn.
initRedraw :: Redraw
initRedraw = Redraw S.empty False False

-- | Reset the redraw state, by setting it to 'initRedraw'.  Typically
--   this should be called right after drawing each frame, to be ready
--   to collect redrawing information for the next frame.
resetRedraw :: Redraw -> Redraw
resetRedraw = const initRedraw

-- | Does the world pane need redrawing at all?
needsRedraw :: Redraw -> Bool
needsRedraw r =
  or
    [ r ^. redrawWorld
    , r ^. drawFrame
    , not (S.null $ r ^. dirtyCells)
    ]
