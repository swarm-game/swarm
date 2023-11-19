{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for generating doc markup
module Swarm.Doc.Util where

import Data.Text (Text)
import Data.Text qualified as T

wrap :: Char -> Text -> Text
wrap c = T.cons c . flip T.snoc c

codeQuote :: Text -> Text
codeQuote = wrap '`'

addLink :: Text -> Text -> Text
addLink l t = T.concat ["[", t, "](", l, ")"]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
