-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A special modified version of 'Brick.Widgets.List.handleListEvent'
-- to deal with skipping over separators.
module Brick.Widget.List.Skippable (
  Amount (..),
  Jump (..),
  Dir (..),
  navigateList,
) where

import Brick (EventM)
import Brick.Widgets.List qualified as BL
import Control.Lens (set, (&), (^.))
import Control.Monad.State.Strict (modify)
import Data.Foldable (toList)
import Data.List (find)
import Graphics.Vty qualified as V
import Swarm.Util (cycleEnum)

-- | The amount to move
data Amount
  = Single Dir
  | Multi Dir Jump
  | None

-- | How far to jump for multiple-row movement
data Jump = Max | Page

-- | Which direction to search: forward or backward from the current location.
data Dir = Down | Up deriving (Eq, Ord, Show, Enum, Bounded)

invert :: Dir -> Dir
invert = cycleEnum

-- | Handle a list event, taking an extra predicate to identify which
--   list elements are separators; separators will be skipped if
--   possible.
--
-- Supply a pure function that maps events to the movement amount.
navigateList ::
  (Ord n, Foldable t, BL.Splittable t) =>
  -- | separator predicate
  (e -> Bool) ->
  V.Event ->
  -- | movement amount mapping
  (V.Event -> Amount) ->
  EventM n (BL.GenericList n t e) ()
navigateList isSep e evMap = case evMap e of
  Single d -> modify $ f d ExcludeCurrent
  Multi d stride ->
    let g = f (invert d) IncludeCurrent
     in case stride of
          Max ->
            modify $
              g . case d of
                Up -> BL.listMoveToBeginning
                Down -> BL.listMoveToEnd
          Page -> precondition >> modify g
           where
            precondition = case d of
              Up -> BL.listMovePageUp
              Down -> BL.listMovePageDown
  None -> return ()
 where
  f d x = listFindByStrategy (FindStrategy d x) $ not . isSep

-- | Should we include or exclude the current location in the search?
data FindStart = IncludeCurrent | ExcludeCurrent deriving (Eq, Ord, Show, Enum)

-- | A 'FindStrategy' is a pair of a 'FindDir' and a 'FindStart'.
data FindStrategy = FindStrategy Dir FindStart

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
        case dir of Down -> 0; Up -> 1
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
      result = case dir of Down -> tailResult; Up -> headResult
   in maybe id (set BL.listSelectedL . Just . fst) result l
