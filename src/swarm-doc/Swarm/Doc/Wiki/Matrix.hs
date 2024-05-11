{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of command attributes matrix.
module Swarm.Doc.Wiki.Matrix where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Swarm.Doc.Command
import Text.Pandoc
import Text.Pandoc.Builder

commandsMatrix :: Pandoc
commandsMatrix =
  setTitle (text "Commands matrix") $
    doc (header 3 (text "Commands matrix"))
      <> doc (makePropsTable ["Command", "Effects", "Actor Target", "Type"])

makePropsTable ::
  [T.Text] ->
  Blocks
makePropsTable headingsList =
  simpleTable headerRow $ map genPropsRow catalogEntries
 where
  CommandCatalog catalogEntries = getCatalog
  headerRow = map (plain . text) headingsList

genPropsRow :: CommandEntry -> [Blocks]
genPropsRow e =
  [ showCode (cmd e)
  , showCode (effects e)
  , showCode (hasActorTarget $ derivedAttrs e)
  ]
    <> NE.toList completeTypeMembers
 where
  showCode :: Show a => a -> Blocks
  showCode = plain . code . T.pack . show
  completeTypeMembers = NE.map showCode $ argTypes e
