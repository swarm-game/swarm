{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstracting over input source (stdin or file).
module Swarm.Util.InputSource where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Swarm.Util (Encoding (..), readFileMayT)

-- | From where should the input be taken?
data InputSource = Stdin | InputFile FilePath

-- | Fetch input from the indicated location.
getInput :: InputSource -> IO (Maybe Text)
getInput Stdin = Just <$> T.getContents
getInput (InputFile fp) = readFileMayT SystemLocale fp

-- | Display an input source.
showInput :: InputSource -> Text
showInput Stdin = "(input)"
showInput (InputFile fp) = T.pack fp
