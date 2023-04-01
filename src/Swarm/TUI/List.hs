-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A special modified version of 'Brick.Widgets.List.handleListEvent'
-- to deal with skipping over separators.
module Swarm.TUI.List (handleListEventWithSeparators) where

import Brick (EventM)
import Brick.Widget.List.Skippable
import Brick.Widgets.List qualified as BL
import Graphics.Vty qualified as V

-- | Handle a list event, taking an extra predicate to identify which
--   list elements are separators; separators will be skipped if
--   possible.
handleListEventWithSeparators ::
  (Foldable t, BL.Splittable t, Ord n) =>
  V.Event ->
  -- | Is this element a separator?
  (e -> Bool) ->
  EventM n (BL.GenericList n t e) ()
handleListEventWithSeparators e isSep =
  navigateList isSep eventMap e
 where
  eventMap = \case
    V.EvKey V.KUp [] -> Single Up
    V.EvKey (V.KChar 'k') [] -> Single Up
    V.EvKey V.KDown [] -> Single Down
    V.EvKey (V.KChar 'j') [] -> Single Down
    V.EvKey V.KHome [] -> Multi Max Up
    V.EvKey V.KEnd [] -> Multi Max Down
    V.EvKey V.KPageDown [] -> Multi Page Down
    V.EvKey V.KPageUp [] -> Multi Page Up
    _ -> None
