-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Preserve color fidelity for non-TUI rendering
module Swarm.TUI.View.Attribute.Color where

import Brick qualified as B
import Data.Colour.SRGB (RGB (..))
import Graphics.Vty qualified as V
import Swarm.Game.Entity.Cosmetic (HiFiColor (..))

-- | Includes both a VTY-specific style specification and a
-- high-fidelity color specification for rendering to other mediums.
--
-- Preferably, in most cases a single source of truth serves to specify
-- a color upstream, and the other of the two specifications is derived,
-- using the utility functions in this module.
data EntityStyle = EntityStyle
  { vtyColor :: V.Attr
  -- ^ Includes foreground, background, and styling
  , niceColor :: HiFiColor
  }

fromHiFi :: HiFiColor -> EntityStyle
fromHiFi hifi = EntityStyle v hifi
 where
  v = case hifi of
    FgOnly c -> B.fg $ mkBrickColor c
    BgOnly c -> B.bg $ mkBrickColor c
    FgAndBg foreground background -> mkBrickColor foreground `B.on` mkBrickColor background

  mkBrickColor (RGB r g b) = V.RGBColor r g b
