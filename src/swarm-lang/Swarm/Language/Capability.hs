{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Capabilities needed to evaluate and execute programs.  Language
-- constructs or commands require certain capabilities, and in turn
-- capabilities are provided by various devices.  A robot must have an
-- appropriate device equipped in order to make use of each language
-- construct or command.
module Swarm.Language.Capability (
  Capability (..),
  capabilityName,
  parseCapability,
  constCaps,
  constByCaps,
) where

import Control.Arrow ((&&&))
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Foldable (find)
import Data.Hashable (Hashable)
import Data.List.Extra (enumerate)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Yaml
import GHC.Generics (Generic)
import Generic.Data (FiniteEnumeration (..))
import Prettyprinter (pretty)
import Swarm.Language.Syntax.Constants (Const (..), allConst, constInfo, syntax)
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Util (binTuples, failT, showEnum)
import Witch (from)
import Prelude hiding (lookup)

-- | Various capabilities which robots can have.
data Capability
  = -- | Execute the command or function.
    CExecute Const
  | -- | Be powered, i.e. execute anything at all
    CPower
  | -- | Allow a heavy robot to perform movements (e.g. move, backup and stride).
    CMoveHeavy
  | -- | Don't drown in liquid.
    CFloat
  | -- | Allow using absolute directions.
    COrient
  | -- | Store and look up definitions in an environment
    CEnv
  | -- | Interpret lambda abstractions
    CLambda
  | -- | Enable recursive definitions
    CRecursion
  | -- | Capability for working with sum types.
    CSum
  | -- | Capability for working with product types.
    CProd
  | -- | Capability for working with record types.
    CRecord
  | -- | Debug capability.
    CDebug
  | -- | Capability to handle recursive types.
    CRectype
  | -- | God-like capabilities.  For e.g. commands intended only for
    --   checking challenge mode win conditions, and not for use by
    --   players.
    CGod
  deriving (Eq, Ord, Show, Generic, Hashable, Data, FromJSONKey, ToJSONKey)
  deriving (Enum, Bounded) via (FiniteEnumeration Capability)

instance PrettyPrec Capability where
  prettyPrec _ c = pretty $ T.toLower (from (NE.tail $ showEnum c))

-- | Get the name of the capability for use in UI and YAML.
capabilityName :: Capability -> Text
capabilityName = \case
  CExecute con -> case con of
    Neg -> "neg"
    _ -> syntax $ constInfo con
  CMoveHeavy -> "move heavy robot"
  cap -> from @String . map toLower . drop 1 $ show cap

-- | Parse the capability name - inverse of 'capabilityName'.
--
-- >>> import Data.List.Extra (enumerate)
-- >>> all (\c -> Just c == parseCapability (capabilityName c)) enumerate
-- True
parseCapability :: Text -> Maybe Capability
parseCapability t = find (\c -> capabilityName c == T.toLower t) enumerate

instance ToJSON Capability where
  toJSON = String . capabilityName

instance FromJSON Capability where
  parseJSON = withText "Capability" tryRead
   where
    tryRead :: Text -> Parser Capability
    tryRead t = case parseCapability t of
      Just c -> return c
      Nothing -> failT ["Unknown capability", t]

-- | Capabilities needed to evaluate or execute a constant.
constCaps :: Const -> Maybe Capability
constCaps = \case
  -- ----------------------------------------------------------------
  -- Some built-in constants that don't require any special capability.
  AppF -> Nothing
  Base -> Nothing
  Equipped -> Nothing
  Fail -> Nothing
  Force -> Nothing
  Has -> Nothing
  Knows -> Nothing
  Noop -> Nothing
  Parent -> Nothing
  Pure -> Nothing
  Say -> Nothing -- speaking is natural to robots (unlike listening)
  Setname -> Nothing
  Undefined -> Nothing
  Use -> Nothing -- Recipes alone shall dictate whether things can be "used"
  View -> Nothing -- TODO: #17 should require equipping an antenna
  -- Some God-like abilities.
  As -> Just CGod
  Create -> Just CGod
  Instant -> Just CGod
  RobotNamed -> Just CGod
  RobotNumbered -> Just CGod
  Surveil -> Just CGod
  Destroy -> Just CGod
  -- ----------------------------------------------------------------
  -- type-level arithmetic
  Inl -> Just CSum
  Inr -> Just CSum
  Case -> Just CSum
  -- TODO: #563 pair syntax (1,2,3...) should require CProd too
  Match -> Just CProd
  c -> Just (CExecute c)

-- | Inverts the 'constCaps' mapping.
constByCaps :: Map Capability (NE.NonEmpty Const)
constByCaps =
  binTuples $
    map swap $
      mapMaybe (sequenceA . (id &&& constCaps)) allConst
