-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: TUI rendering of entities
--
-- A "texel" ("textual element") represent concrete information about
-- how to draw the foreground and/or background of one textual cell,
-- together with priority values.  Texels for multiple things in the
-- same cell can be combined to appropriately compute what to draw.
module Swarm.Game.Cosmetic.Texel (
  Priority,
  Texel(..),
  mkTexel,
  getTexelData,
  getTexelColor,
  texelFromColor,
  texelIsEmpty,
) where

import Data.Maybe (isNothing)
import Data.Semigroup (Arg(..), ArgMax, Max(..))
import Swarm.Game.Cosmetic.Color

-- | Display priority.  Entities with higher priority will be drawn on
--   top of entities with lower priority.
type Priority = Int

-- | A 'textual element', specifying exactly how to draw a single
--   cell.  Polymorphic in the attribute/color type.
data Texel a = Texel
  { _texelFG :: Maybe (ArgMax Priority (Char, a))
  , _texelBG :: Maybe (ArgMax Priority a)
  }
  deriving (Eq, Show)

mkTexel :: Maybe (Priority, (Char, a)) -> Maybe (Priority, a) -> Texel a
mkTexel fg bg = Texel (Max . uncurry Arg <$> fg) (Max . uncurry Arg <$> bg)

getTexelData :: Texel a -> (Maybe (Char, a), Maybe a)
getTexelData (Texel fg bg) = (getArg . getMax <$> fg, getArg . getMax <$> bg)
  where
    getArg (Arg _ a) = a

-- | Extract the color (foreground + background) of a texel.
getTexelColor :: Texel a -> Maybe (ColorLayers a)
getTexelColor t = case getTexelData t of
  (fg, bg) -> mkColorLayers (snd <$> fg) bg

texelFromColor :: Priority -> Char -> ColorLayers a -> Texel a
texelFromColor prio c cl = case cl of
  FgOnly fg -> mkTexel (Just (prio, (c, fg))) Nothing
  BgOnly bg -> mkTexel Nothing (Just (prio, bg))
  FgAndBg fg bg -> mkTexel (Just (prio, (c, fg))) (Just (prio, bg))

texelIsEmpty :: Texel a -> Bool
texelIsEmpty (Texel fg bg) = isNothing fg && isNothing bg

instance Semigroup (Texel a) where
  Texel f1 b1 <> Texel f2 b2 = Texel (f1 <> f2) (b1 <> b2)

instance Monoid (Texel a) where
  mempty = Texel mempty mempty
