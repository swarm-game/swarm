-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for generating doc markup
module Swarm.Doc.Wiki.Util where

import Control.Arrow (left)
import Data.Text (Text)
import Text.Pandoc

pandocToText :: Pandoc -> Either Text Text
pandocToText =
  left renderError
    . runPure
    . writeMarkdown (def {writerExtensions = extensionsFromList [Ext_pipe_tables]})
