-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: TUI rendering of entities
--
-- Utilities for describing how to display in-game entities in the TUI.
module Swarm.Game.Texel (
  Priority,
  Texel(..),
  mkTexel,
  getTexelChar,
  getTexelData,
  getTexelAttr,
) where

import Control.Applicative ((<|>))
import Data.Semigroup (Arg(..), ArgMax, Max(..))

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

-- | XXX get FG char if exists
getTexelChar :: Texel a -> Maybe Char
getTexelChar = fmap fst . fst . getTexelData

-- | XXX Get FG if it exists, else BG
getTexelAttr :: Texel a -> Maybe a
getTexelAttr t = case getTexelData t of
  (fg, bg) -> (snd <$> fg) <|> bg

instance Semigroup (Texel a) where
  Texel f1 b1 <> Texel f2 b2 = Texel (f1 <> f2) (b1 <> b2)

instance Monoid (Texel a) where
  mempty = Texel mempty mempty
