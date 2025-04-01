{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Here is the WorldPanel player configurable key event handler.
module Swarm.TUI.Controller.EventHandlers.World (
  worldEventHandlers,
) where

import Brick hiding (Location)
import Brick.Keybindings
import Control.Lens
import Control.Monad (when)
import Data.Int (Int32)
import Linear
import Swarm.Game.Location
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Language.Syntax.Direction (Direction (..), directionSyntax)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.Event

-- | Handle a user input event in the world view panel.
worldEventHandlers :: [KeyEventHandler SwarmEvent (EventM Name AppState)]
worldEventHandlers = allHandlers World $ \case
  MoveViewEvent d -> ("Scroll world view in the " <> directionSyntax (DAbsolute d) <> " direction", scrollViewInDir $ toHeading d)

scrollViewInDir :: V2 Int32 -> EventM Name AppState ()
scrollViewInDir d = do
  c <- use $ playState . gameState . creativeMode
  s <- use $ playState . gameState . landscape . worldScrollable
  when (c || s) $ scrollView (.+^ (worldScrollDist *^ d))

worldScrollDist :: Int32
worldScrollDist = 8

-- | Manually scroll the world view.
scrollView :: (Location -> Location) -> EventM Name AppState ()
scrollView update = do
  -- Manually invalidate the 'WorldCache' instead of just setting
  -- 'needsRedraw'.  I don't quite understand why the latter doesn't
  -- always work, but there seems to be some sort of race condition
  -- where 'needsRedraw' gets reset before the UI drawing code runs.
  invalidateCacheEntry WorldCache
  playState . gameState . robotInfo %= modifyViewCenter (fmap update)
