-- |
-- Module      :  Swarm.TUI.List
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A special modified version of 'Brick.Widgets.List.handleListEvent'
-- to deal with skipping over separators.
module Swarm.TUI.List (handleListEventWithSeparators) where

import Control.Lens (set, (&), (^.))
import Data.Foldable (toList)
import Data.List (find)

import Brick (EventM)
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as V

-- | Handle a list event, taking an extra predicate to identify which
--   list elements are separators; separators will be skipped if
--   possible.
handleListEventWithSeparators ::
  (Foldable t, BL.Splittable t, Ord n) =>
  V.Event ->
  -- | Is this element a separator?
  (e -> Bool) ->
  BL.GenericList n t e ->
  EventM n (BL.GenericList n t e)
handleListEventWithSeparators e isSep theList =
  case e of
    V.EvKey V.KUp [] -> return backward
    V.EvKey (V.KChar 'k') [] -> return backward
    V.EvKey V.KDown [] -> return forward
    V.EvKey (V.KChar 'j') [] -> return forward
    V.EvKey V.KHome [] ->
      return $
        listFindByStrategy fwdInclusive isItem $
          BL.listMoveToBeginning theList
    V.EvKey V.KEnd [] ->
      return $
        listFindByStrategy bwdInclusive isItem $
          BL.listMoveToEnd theList
    V.EvKey V.KPageDown [] ->
      listFindByStrategy bwdInclusive isItem <$> BL.listMovePageDown theList
    V.EvKey V.KPageUp [] ->
      listFindByStrategy fwdInclusive isItem <$> BL.listMovePageUp theList
    _ -> return theList
 where
  isItem = not . isSep
  backward = listFindByStrategy bwdExclusive isItem theList
  forward = listFindByStrategy fwdExclusive isItem theList

-- | Which direction to search: forward or backward from the current location.
data FindDir = FindFwd | FindBwd deriving (Eq, Ord, Show, Enum)

-- | Should we include or exclude the current location in the search?
data FindStart = IncludeCurrent | ExcludeCurrent deriving (Eq, Ord, Show, Enum)

-- | A 'FindStrategy' is a pair of a 'FindDir' and a 'FindStart'.
data FindStrategy = FindStrategy FindDir FindStart

-- | Some convenient synonyms for various 'FindStrategy' values.
fwdInclusive, fwdExclusive, bwdInclusive, bwdExclusive :: FindStrategy
fwdInclusive = FindStrategy FindFwd IncludeCurrent
fwdExclusive = FindStrategy FindFwd ExcludeCurrent
bwdInclusive = FindStrategy FindBwd IncludeCurrent
bwdExclusive = FindStrategy FindBwd ExcludeCurrent

-- | Starting from the currently selected element, attempt to find and
--   select the next element matching the predicate. How the search
--   proceeds depends on the 'FindStrategy': the 'FindDir' says
--   whether to search forward or backward from the selected element,
--   and the 'FindStart' says whether the currently selected element
--   should be included in the search or not.
listFindByStrategy ::
  (Foldable t, BL.Splittable t) =>
  FindStrategy ->
  (e -> Bool) ->
  BL.GenericList n t e ->
  BL.GenericList n t e
listFindByStrategy (FindStrategy dir cur) test l =
  -- Figure out what index to split on.  We will call splitAt on
  -- (current selected index + adj).
  let adj =
        -- If we're search forward, split on current index; if
        -- finding backward, split on current + 1 (so that the
        -- left-hand split will include the current index).
        case dir of FindFwd -> 0; FindBwd -> 1
          -- ... but if we're excluding the current index, swap that, so
          -- the current index will be excluded rather than included in
          -- the part of the split we're going to look at.
          & case cur of IncludeCurrent -> id; ExcludeCurrent -> (1 -)

      -- Split at the index we computed.
      start = maybe 0 (+ adj) (l ^. BL.listSelectedL)
      (h, t) = BL.splitAt start (l ^. BL.listElementsL)

      -- Now look at either the right-hand split if searching
      -- forward, or the reversed left-hand split if searching
      -- backward.
      headResult = find (test . snd) . reverse . zip [0 ..] . toList $ h
      tailResult = find (test . snd) . zip [start ..] . toList $ t
      result = case dir of FindFwd -> tailResult; FindBwd -> headResult
   in maybe id (set BL.listSelectedL . Just . fst) result l
