-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing utilities for Swarm.
module Swarm.Util.Parse where

import Control.Applicative (optional)
import Text.Megaparsec (MonadParsec, eof)

-- | Run a parser "fully", consuming leading whitespace and ensuring
--   that the parser extends all the way to eof.
fully :: (MonadParsec e s f) => f () -> f a -> f a
fully sc p = sc *> p <* eof

-- | Run a parser "fully", consuming leading whitespace (including the
--   possibility that the input is nothing but whitespace) and
--   ensuring that the parser extends all the way to eof.
fullyMaybe :: (MonadParsec e s f) => f () -> f a -> f (Maybe a)
fullyMaybe sc = fully sc . optional
