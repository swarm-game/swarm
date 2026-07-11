{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering Markdown documents as text.
--
-- For rendering to brick widgets, see
-- 'Swarm.TUI.View.Util.drawMarkdown'.
module Swarm.Text.Markdown.Render (
  -- * Token stream -> text conversion
  tokenToText,
  streamToText,

  -- * Rendering streamable things as text
  toText,
  toTextWidth,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Pretty (PrettyPrec)
import Swarm.Text.Markdown.Document (Document)
import Swarm.Text.Markdown.Layout (documentToStream)
import Swarm.Text.Markdown.Token (
  OutputToken,
  Token' (..),
 )

------------------------------------------------------------
-- Token stream -> text conversion
------------------------------------------------------------

tokenToText :: OutputToken -> Text
tokenToText = \case
  TextToken t -> t
  Newline -> "\n"
  Para -> "\n\n"
  PushAttr _ -> ""
  PopAttr -> ""

streamToText :: [OutputToken] -> Text
streamToText = T.concat . map tokenToText

------------------------------------------------------------
-- Rendering streamable things as text
------------------------------------------------------------

-- | Turn a 'Document' into text, ignoring any formatting.
toText :: PrettyPrec c => Document c -> Text
toText = toTextWidth Nothing

-- | Turn a 'Document' into text, ignoring any formatting but wrapping
--   to a specified line width.
toTextWidth :: PrettyPrec c => Maybe Int -> Document c -> Text
toTextWidth mw = streamToText . documentToStream mw
