-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Auto-generation of command attributes matrix.
module Swarm.Doc.Command where

import Data.Aeson (ToJSON)
import Data.List.Extra (enumerate)
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Servant.Docs qualified as SD
import Swarm.Doc.Util
import Swarm.Language.Syntax
import Swarm.Language.Syntax.CommandMetadata
import Swarm.Language.Typecheck (inferConst)
import Swarm.Language.Types

data DerivedAttrs = DerivedAttrs
  { hasActorTarget :: Bool
  , pureComputation :: Bool
  , modifiesEnvironment :: Bool
  , modifiesRobot :: Bool
  , movesRobot :: Bool
  , returnsValue :: Bool
  , outputType :: String
  }
  deriving (Generic, ToJSON)

data CommandEntry = CommandEntry
  { cmd :: Const
  , effects :: Set CommandEffect
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

-- | Uses explicit effects documentation as well as
-- type signature information to compute various flags
mkEntry :: Const -> CommandEntry
mkEntry c =
  CommandEntry c cmdEffects rawArgs $
    DerivedAttrs
      { hasActorTarget = operatesOnActor inputArgs
      , pureComputation = Set.null cmdEffects
      , modifiesEnvironment = Mutation EntityChange `Set.member` cmdEffects
      , modifiesRobot = not . Set.disjoint cmdEffects . Set.fromList $ map (Mutation . RobotChange) enumerate
      , movesRobot = Mutation (RobotChange PositionChange) `Set.member` cmdEffects
      , returnsValue = theOutputType /= TyCmd TyUnit
      , outputType = show theOutputType
      }
 where
  cmdInfo = constInfo c
  cmdEffects = effectInfo $ constDoc cmdInfo

  getArgs = unchainFun . ptBody

  rawArgs = getArgs $ inferConst c

  inputArgs = NE.init rawArgs
  theOutputType = NE.last rawArgs

  operatesOnActor = elem TyActor

getCatalog :: CommandCatalog
getCatalog = CommandCatalog $ map mkEntry commands
