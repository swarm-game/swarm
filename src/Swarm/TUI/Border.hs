-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Swarm.TUI.Border
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Special border drawing functions that can include labels in more
-- places than just the top center.
module Swarm.TUI.Border (
  -- * Horizontal border labels
  HBorderLabels,
  plainHBorder,
  leftLabel,
  centerLabel,
  rightLabel,

  -- * Rectangular border labels
  BorderLabels,
  plainBorder,
  topLabels,
  bottomLabels,

  -- * Border-drawing functions
  hBorderWithLabels,
  borderWithLabels,
) where

import Brick
import Brick.Widgets.Border
import Control.Lens (makeLenses, to, (^.))
import qualified Graphics.Vty as V

-- | Labels for a horizontal border, with optional left, middle, and
--   right labels.
data HBorderLabels n = HBorderLabels
  { _leftLabel :: Maybe (Widget n)
  , _centerLabel :: Maybe (Widget n)
  , _rightLabel :: Maybe (Widget n)
  }

-- | A plain horizontal border with no labels.
plainHBorder :: HBorderLabels n
plainHBorder = HBorderLabels Nothing Nothing Nothing

-- | Labels for a rectangular border, with optional left, middle, and
--   right labels on the top and bottom.
data BorderLabels n = BorderLabels
  { _topLabels :: HBorderLabels n
  , _bottomLabels :: HBorderLabels n
  }

-- | A plain rectangular border with no labels.
plainBorder :: BorderLabels n
plainBorder = BorderLabels plainHBorder plainHBorder

makeLenses ''HBorderLabels
makeLenses ''BorderLabels

-- | Draw a horizontal border with three optional labels.  The left
--   label (if present) will be placed two units away from the left
--   end of the border, and the right label will be placed two units
--   away from the right end.  The center label, if present, will
--   always be centered in the border overall, regardless of the width
--   of the left and right labels.  This ensures that when the labels
--   change width, they do not cause the other labels to wiggle.
hBorderWithLabels ::
  HBorderLabels n -> Widget n
hBorderWithLabels (HBorderLabels l c r) =
  Widget Greedy Fixed $ do
    let renderLabel = render . maybe emptyWidget (vLimit 1)
    rl <- renderLabel l
    rc <- renderLabel c
    rr <- renderLabel r

    -- Figure out how wide the whole border is supposed to be
    ctx <- getContext
    let w = ctx ^. availWidthL

        -- Get the widths of the labels
        lw = V.imageWidth (image rl)
        cw = V.imageWidth (image rc)

    -- Now render the border with labels.
    render $
      hBox
        [ hLimit 2 hBorder
        , Widget Fixed Fixed (return rl)
        , -- We calculate the specific width of border between the left
          -- and center labels needed to ensure that the center label is
          -- in the right place.  Note, using (cw + 1) `div` 2, as
          -- opposed to cw `div` 2, means that the placement of the
          -- center label will be left-biased: if it does not fit
          -- exactly at the center it will be placed just to the left of
          -- center.
          hLimit (w `div` 2 - 2 - lw - (cw + 1) `div` 2) hBorder
        , Widget Fixed Fixed (return rc)
        , -- The border between center and right greedily fills up any
          -- remaining width.
          hBorder
        , Widget Fixed Fixed (return rr)
        , hLimit 2 hBorder
        ]

-- | Put a rectangular border around the specified widget with the
--   specified label widgets placed around the border.
borderWithLabels :: BorderLabels n -> Widget n -> Widget n
borderWithLabels labels wrapped =
  Widget (hSize wrapped) (vSize wrapped) $ do
    c <- getContext

    middleResult <-
      render $
        hLimit (c ^. availWidthL - 2) $
          vLimit (c ^. availHeightL - 2) $
            wrapped

    let tl = joinableBorder (Edges False True False True)
        tr = joinableBorder (Edges False True True False)
        bl = joinableBorder (Edges True False False True)
        br = joinableBorder (Edges True False True False)
        top = tl <+> hBorderWithLabels (labels ^. topLabels) <+> tr
        bottom = bl <+> hBorderWithLabels (labels ^. bottomLabels) <+> br
        middle = vBorder <+> Widget Fixed Fixed (return middleResult) <+> vBorder
        total = top <=> middle <=> bottom

    render $
      hLimit (middleResult ^. imageL . to V.imageWidth + 2) $
        vLimit (middleResult ^. imageL . to V.imageHeight + 2) $
          total
