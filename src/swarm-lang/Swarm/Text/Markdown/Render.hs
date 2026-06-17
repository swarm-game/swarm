{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering Markdown documents as text.
--
-- For rendering to brick widgets, see
-- 'Swarm.TUI.View.Util.drawMarkdown'.
module Swarm.Text.Markdown.Render where

import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Text.Markdown.Layout (ToStream (..))
import Swarm.Text.Markdown.Token (
  OutputToken,
  Token' (..),
  TokenPhase (Output),
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

-- | Turn anything that can be converted to a token stream (such as
--   'Document') into text, ignoring any formatting.
toText :: ToStream a Output => a -> Text
toText = toTextWidth Nothing

-- | Turn anything that can be converted to a token stream (such as
--   'Document') into text, ignoring any formatting but wrapping to a
--   specified line width.
toTextWidth :: ToStream a Output => Maybe Int -> a -> Text
toTextWidth mw = streamToText . toStream mw
