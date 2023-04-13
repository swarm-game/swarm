-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A special modified version of 'Brick.Widgets.List.handleListEvent'
-- to deal with skipping over separators.
module Swarm.TUI.List (handleListEventWithSeparators) where

import Brick (EventM)
import Brick.Widgets.List qualified as BL
import Graphics.Vty qualified as V
import Brick.Widgets.List.Skip

-- | Handle a list event, taking an extra predicate to identify which
--   list elements are separators; separators will be skipped if
--   possible.
handleListEventWithSeparators ::
  (Foldable t, BL.Splittable t, Ord n, Searchable t) =>
  V.Event ->
  -- | Is this element a separator?
  (e -> Bool) ->
  EventM n (BL.GenericList n t e) ()
handleListEventWithSeparators e isSep =
  listSkip isSep movement
 where
  movement = case e of
    V.EvKey V.KUp [] -> Move One Bwd
    V.EvKey (V.KChar 'k') [] -> Move One Bwd
    V.EvKey V.KDown [] -> Move One Fwd
    V.EvKey (V.KChar 'j') [] -> Move One Fwd
    V.EvKey V.KHome [] -> Move Most Bwd
    V.EvKey V.KEnd [] -> Move Most Fwd
    V.EvKey V.KPageDown [] -> Move Page Fwd
    V.EvKey V.KPageUp [] -> Move Page Bwd
    _ -> NoMove
