{-# LANGUAGE OverloadedStrings #-}

-- |
-- Generation of "static" when robots are out of range.
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Static where

import Control.Lens ((^.))
import Data.ByteString (ByteString)
import Data.Colour.SRGB (RGB (..))
import Data.Hash.Murmur (murmur3)
import Data.Tagged (unTagged)
import Data.Word (Word32)
import Linear.Affine ((.-.))
import Swarm.Game.Cosmetic.Color (NamedColor (..), TrueColor (..))
import Swarm.Game.Cosmetic.Texel (Texel, mkTexel)
import Swarm.Game.State
import Swarm.Game.State.Robot (viewCenter)
import Swarm.Game.State.Substate (ticks)
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Game.Universe (planar)
import Swarm.Game.World.Coords (Coords, coordsToLoc)
import Witch (from)
import Witch.Encoding qualified as Encoding

-- | Random "static" based on the distance to the robot being
--   @view@ed.
renderStaticAt :: GameState -> Coords -> Texel TrueColor
renderStaticAt g coords = maybe mempty renderStatic (getStatic g coords)

-- | Draw static given a number from 0-15 representing the state of
--   the four quarter-pixels in a cell
renderStatic :: Word32 -> Texel TrueColor
renderStatic s =
  mkTexel
    (Just (maxBound, (staticChar s, AnsiColor White)))
    (Just (maxBound, Triple $ RGB 0 0 0))

-- | Given a value from 0--15, considered as 4 bits, pick the
--   character with the corresponding quarter pixels turned on.
staticChar :: Word32 -> Char
staticChar = \case
  0 -> ' '
  1 -> '▖'
  2 -> '▗'
  3 -> '▄'
  4 -> '▘'
  5 -> '▌'
  6 -> '▚'
  7 -> '▙'
  8 -> '▝'
  9 -> '▞'
  10 -> '▐'
  11 -> '▟'
  12 -> '▀'
  13 -> '▛'
  14 -> '▜'
  15 -> '█'
  _ -> ' '

-- | Random "static" based on the distance to the robot being
--   @view@ed.  A cell can either be static-free (represented by
--   @Nothing@) or can have one of sixteen values (representing the
--   state of the four quarter-pixels in one cell).
getStatic :: GameState -> Coords -> Maybe Word32
getStatic g coords
  | isStatic = Just (h `mod` 16)
  | otherwise = Nothing
 where
  -- Offset from the location of the view center to the location under
  -- consideration for display.
  offset = coordsToLoc coords .-. (g ^. robotInfo . viewCenter . planar)

  -- Hash.
  h =
    murmur3 1 . unTagged . from @String @(Encoding.UTF_8 ByteString) . show $
      -- include the current tick count / 16 in the hash, so the pattern of static
      -- changes once every 16 ticks
      (offset, getTickNumber (g ^. temporal . ticks) `div` 16)

  -- Hashed probability, i.e. convert the hash into a floating-point number between 0 and 1
  hp :: Double
  hp = fromIntegral h / fromIntegral (maxBound :: Word32)

  isStatic = case focusedRange g of
    -- If we're not viewing a robot, display static.  This
    -- can happen if e.g. the robot we were viewing drowned.
    -- This is overridden by creative mode, e.g. when no robots
    -- have been defined for the scenario.
    Nothing -> not $ g ^. creativeMode
    -- Don't display static if the robot is close, or when we're in
    -- creative mode or the player is allowed to scroll the world.
    Just Close -> False
    -- At medium distances, replace cell with static with a
    -- probability that increases with distance.
    Just (MidRange s) -> hp < 1 - cos (s * (pi / 2))
    -- Far away, everything is static.
    Just Far -> True
