-- |
-- Module      :  Swarm.Language.Capability
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Capabilities needed to evaluate and execute programs.  Language
-- constructs or commands require certain capabilities, and in turn
-- capabilities are provided by various devices.  A robot must have an
-- appropriate device installed in order to make use of each language
-- construct or command.
module Swarm.Language.Capability (
  Capability (..),
  capabilityName,
  constCaps,
) where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Char (toLower)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)
import Witch (from)
import Prelude hiding (lookup)

import Data.Data (Data)
import Data.Yaml
import GHC.Generics (Generic)

import Swarm.Language.Syntax

-- | Various capabilities which robots can have.
data Capability
  = -- | Be powered, i.e. execute anything at all
    CPower
  | -- | Execute the 'Move' command
    CMove
  | -- | Execute the 'Turn' command
    --
    -- NOTE: using cardinal directions is separate 'COrient' capability
    CTurn
  | -- | Execute the 'Selfdestruct' command
    CSelfdestruct
  | -- | Execute the 'Grab' command
    CGrab
  | -- | Execute the 'Harvest' command
    CHarvest
  | -- | Execute the 'Place' command
    CPlace
  | -- | Execute the 'Give' command
    CGive
  | -- | Execute the 'Install' command
    CInstall
  | -- | Execute the 'Make' command
    CMake
  | -- | Execute the 'Count' command
    CCount
  | -- | Execute the 'Build' command
    CBuild
  | -- | Execute the 'Salvage' command
    CSalvage
  | -- | Execute the 'Drill' command
    CDrill
  | -- | Execute the 'Whereami' command
    CSenseloc
  | -- | Execute the 'Blocked' command
    CSensefront
  | -- | Execute the 'Ishere' command
    CSensehere
  | -- | Execute the 'Scan' command
    CScan
  | -- | Execute the 'Random' command
    CRandom
  | -- | Execute the 'Appear' command
    CAppear
  | -- | Execute the 'Create' command
    CCreate
  | -- | Execute the 'Log' command
    CLog
  | -- | Don't drown in liquid
    CFloat
  | -- | Evaluate conditional expressions
    CCond
  | -- | Negate boolean value
    CNegation
  | -- | Evaluate comparison operations
    CCompare
  | -- | Use cardinal direction constants.
    COrient
  | -- | Evaluate arithmetic operations
    CArith
  | -- | Store and look up definitions in an environment
    CEnv
  | -- | Interpret lambda abstractions
    CLambda
  | -- | Enable recursive definitions
    CRecursion
  | -- | Execute the 'Reprogram' command
    CReprogram
  | -- | Capability to introspect and see its own name
    CWhoami
  | -- | Capability to set its own name
    CSetname
  | -- | Capability to move unrestricted to any place
    CTeleport
  | -- | Capability to run commands atomically
    CAtomic
  | -- | Capabiltiy to do time-related things, like `wait` and get the
    --   current time.
    CTime
  | -- | God-like capabilities.  For e.g. commands intended only for
    --   checking challenge mode win conditions, and not for use by
    --   players.
    CGod
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable, Data, FromJSONKey, ToJSONKey)

capabilityName :: Capability -> Text
capabilityName = from @String . map toLower . drop 1 . show

instance ToJSON Capability where
  toJSON = String . capabilityName

instance FromJSON Capability where
  parseJSON = withText "Capability" tryRead
   where
    tryRead :: Text -> Parser Capability
    tryRead t = case readMaybe . from . T.cons 'C' . T.toTitle $ t of
      Just c -> return c
      Nothing -> fail $ "Unknown capability " ++ from t

-- | Capabilities needed to evaluate or execute a constant.
constCaps :: Const -> Maybe Capability
constCaps = \case
  -- ----------------------------------------------------------------
  -- Some built-in constants that don't require any special capability.
  Noop -> Nothing
  AppF -> Nothing
  Force -> Nothing
  Return -> Nothing
  Parent -> Nothing
  Base -> Nothing
  Setname -> Nothing
  Undefined -> Nothing
  Fail -> Nothing
  Has -> Nothing
  Installed -> Nothing
  -- speaking is natural to robots (unlike listening)
  Say -> Nothing
  -- TODO: #495
  --   the require command will be inlined once the Issue is fixed
  --   so the capabilities of the run commands will be checked instead
  Run -> Nothing
  -- ----------------------------------------------------------------
  -- Some straightforward ones.
  Log -> Just CLog
  Selfdestruct -> Just CSelfdestruct
  Move -> Just CMove
  Turn -> Just CTurn
  Grab -> Just CGrab
  Harvest -> Just CHarvest
  Place -> Just CPlace
  Give -> Just CGive
  Install -> Just CInstall
  Make -> Just CMake
  Count -> Just CCount
  If -> Just CCond
  Blocked -> Just CSensefront
  Scan -> Just CScan
  Ishere -> Just CSensehere
  Upload -> Just CScan
  Build -> Just CBuild
  Salvage -> Just CSalvage
  Reprogram -> Just CReprogram
  Drill -> Just CDrill
  Neg -> Just CArith
  Add -> Just CArith
  Sub -> Just CArith
  Mul -> Just CArith
  Div -> Just CArith
  Exp -> Just CArith
  Whoami -> Just CWhoami
  Self -> Just CWhoami
  Atomic -> Just CAtomic
  Time -> Just CTime
  Wait -> Just CTime
  -- ----------------------------------------------------------------
  -- String operations
  Format -> Just CLog
  Concat -> Just CLog
  -- ----------------------------------------------------------------
  -- Some God-like abilities.
  As -> Just CGod
  RobotNamed -> Just CGod
  RobotNumbered -> Just CGod
  Create -> Just CGod
  -- ----------------------------------------------------------------
  -- arithmetic
  Eq -> Just CCompare
  Neq -> Just CCompare
  Lt -> Just CCompare
  Gt -> Just CCompare
  Leq -> Just CCompare
  Geq -> Just CCompare
  -- ----------------------------------------------------------------
  -- boolean logic
  And -> Just CCond
  Or -> Just CCond
  Not -> Just CNegation
  -- ----------------------------------------------------------------
  -- Some additional straightforward ones, which however currently
  -- cannot be used in classic mode since there is no craftable item
  -- which conveys their capability. TODO: #26
  Teleport -> Just CTeleport -- Some space-time machine like Tardis?
  Appear -> Just CAppear -- paint?
  Whereami -> Just CSenseloc -- GPS?
  Random -> Just CRandom -- randomness device (with bitcoins)?
  -- ----------------------------------------------------------------
  -- Some more constants which *ought* to have their own capability but
  -- currently don't. TODO: #26
  View -> Nothing -- XXX this should also require something.
  Inl -> Nothing -- XXX should require cap for sums
  Inr -> Nothing
  Case -> Nothing
  Fst -> Nothing -- XXX should require cap for pairs
  Snd -> Nothing
  Try -> Nothing -- XXX these definitely need to require something.
  Knows -> Nothing
