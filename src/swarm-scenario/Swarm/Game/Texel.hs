-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: TUI rendering of entities
--
-- Utilities for describing how to display in-game entities in the TUI.
module Swarm.Game.Texel (
  Priority,
  Texel(..),
  getTexelData
) where

import Data.Semigroup (Arg(..), ArgMax, Max(..))
import Graphics.Vty.Attributes (Attr)

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

getTexelData :: Texel a -> (Maybe (Char, a), Maybe a)
getTexelData (Texel fg bg) = (getArg . getMax <$> fg, getArg . getMax <$> bg)
  where
    getArg (Arg _ a) = a

instance Semigroup (Texel a) where
  Texel f1 b1 <> Texel f2 b2 = Texel (f1 <> f2) (b1 <> b2)

instance Monoid (Texel a) where
  mempty = Texel mempty mempty
