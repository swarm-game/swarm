{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of cheat sheets for the wiki.
module Swarm.Doc.Wiki.Matrix where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant.Docs qualified as SD
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Swarm.Doc.Util
import Swarm.Language.Pretty (unchainFun)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types
import Text.Pandoc
import Swarm.Language.Syntax.CommandMetadata
import Text.Pandoc.Builder

data DerivedAttrs = DerivedAttrs {
    hasActorTarget :: Bool
  , pureComputation :: Bool
  } deriving (Generic, ToJSON)

data CommandEntry = CommandEntry {
    cmd :: Const
  , effects :: CommandEffect
  , argTypes :: NE.NonEmpty Type
  , derivedAttrs :: DerivedAttrs
  } deriving (Generic, ToJSON)

newtype CommandCatalog = CommandCatalog {
    entries :: [CommandEntry]
  } deriving (Generic, ToJSON)

instance SD.ToSample CommandCatalog where
  toSamples _ = SD.noSamples

mkEntry :: Const -> CommandEntry
mkEntry c =
  CommandEntry c  cmdEffects rawArgs $
    DerivedAttrs
      (operatesOnActor inputArgs)
      (cmdEffects == Computation)
  where
    cmdInfo = constInfo c
    cmdEffects = effectInfo $ constDoc cmdInfo

    getArgs ((Forall _ t)) = unchainFun t

    rawArgs = getArgs $ inferConst c

    inputArgs = NE.init rawArgs
    outputType = NE.last rawArgs

    operatesOnActor = elem TyActor

getCatalog :: CommandCatalog
getCatalog = CommandCatalog $ map mkEntry commands
  
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
  ] <> NE.toList completeTypeMembers
 where
  showCode :: Show a => a -> Blocks
  showCode = plain . code . T.pack . show
  completeTypeMembers = NE.map showCode $ argTypes e
