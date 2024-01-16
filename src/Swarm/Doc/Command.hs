-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of command attributes matrix.
module Swarm.Doc.Command where

import Data.Aeson (ToJSON)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import Servant.Docs qualified as SD
import Swarm.Doc.Util
import Swarm.Language.Pretty (unchainFun)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.CommandMetadata
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types

data DerivedAttrs = DerivedAttrs
  { hasActorTarget :: Bool
  , pureComputation :: Bool
  }
  deriving (Generic, ToJSON)

data CommandEntry = CommandEntry
  { cmd :: Const
  , effects :: CommandEffect
  , argTypes :: NE.NonEmpty Type
  , derivedAttrs :: DerivedAttrs
  }
  deriving (Generic, ToJSON)

newtype CommandCatalog = CommandCatalog
  { entries :: [CommandEntry]
  }
  deriving (Generic, ToJSON)

instance SD.ToSample CommandCatalog where
  toSamples _ = SD.noSamples

mkEntry :: Const -> CommandEntry
mkEntry c =
  CommandEntry c cmdEffects rawArgs $
    DerivedAttrs
      (operatesOnActor inputArgs)
      (cmdEffects == Computation)
 where
  cmdInfo = constInfo c
  cmdEffects = effectInfo $ constDoc cmdInfo

  getArgs ((Forall _ t)) = unchainFun t

  rawArgs = getArgs $ inferConst c

  inputArgs = NE.init rawArgs
  -- outputType = NE.last rawArgs -- FIXME

  operatesOnActor = elem TyActor

getCatalog :: CommandCatalog
getCatalog = CommandCatalog $ map mkEntry commands
