{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for doc generation
module Swarm.Docs.Util where

import Control.Monad (zipWithM, zipWithM_, (<=<))
import Control.Monad.Except (ExceptT (..), liftIO, runExceptT, withExceptT)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (Text, unpack)
import Swarm.Util (isRightOr, listEnums, quote)

-- ----------------------------------------------------------------------------
-- UTILITY
-- ----------------------------------------------------------------------------

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

guardRight :: Text -> Either Text a -> ExceptT Text IO a
guardRight what i = i `isRightOr` (\e -> "Failed to " <> what <> ": " <> e)

simpleErrorHandle :: ExceptT Text IO a -> IO a
simpleErrorHandle = either (fail . unpack) pure <=< runExceptT
