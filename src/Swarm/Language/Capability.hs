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
  constCaps,
) where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Language.Syntax
import Swarm.Util (failT)
import Text.Read (readMaybe)
import Witch (from)
import Prelude hiding (lookup)

-- | Various capabilities which robots can have.
data Capability
  = -- | Be powered, i.e. execute anything at all
    CPower
  | -- | Execute the 'Move' command
    CMove
  | -- | Execute the 'Push' command
    CPush
  | -- | Execute the 'Stride' command
    CMovemultiple
  | -- | Execute the 'Move' command for a heavy robot
    CMoveheavy
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
  | -- | Execute the 'Equip' command
    CEquip
  | -- | Execute the 'Unequip' command
    CUnequip
  | -- | Execute the 'Make' command
    CMake
  | -- | Execute the 'Count' command
    CCount
  | -- | Execute the 'Scout' command. Reconnaissance along a line in a direction.
    CRecondir
  | -- | Execute the 'Build' command
    CBuild
  | -- | Execute the 'Salvage' command
    CSalvage
  | -- | Execute the 'Act' command
    CAct
  | -- | Execute the 'Drill' command
    CDrill
  | -- | Execute the 'Whereami' command
    CSenseloc
  | -- | Execute the 'Blocked' command
    CSensefront
  | -- | Execute the 'Ishere' and 'Isempty' commands
    CSensehere
  | -- | Execute the 'Detect' command
    CDetectloc
  | -- | Execute the 'Resonate' command
    CDetectcount
  | -- | Execute the 'Sniff' command
    CDetectdistance
  | -- | Execute the 'Chirp' command
    CDetectdirection
  | -- | Execute the 'Watch' command
    CWakeself
  | -- | Execute the 'Scan' command
    CScan
  | -- | Execute the 'Random' command
    CRandom
  | -- | Execute the 'Appear' command
    CAppear
  | -- | Execute the 'Create' command
    CCreate
  | -- | Execute the 'Listen' command and passively log messages if also has 'CLog'
    CListen
  | -- | Execute the 'Log' command
    CLog
  | -- | Manipulate text values
    CText
  | -- | Convert between characters/text and Unicode values
    CCode
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
  | -- | Execute the `meet` and `meetAll` commands.
    CMeet
  | -- | Capability to introspect and see its own name
    CWhoami
  | -- | Capability to set its own name
    CSetname
  | -- | Capability to move unrestricted to any place
    CTeleport
  | -- | Capability to run commands atomically
    CAtomic
  | -- | Capability to execute swap (grab and place atomically at the same time).
    CSwap
  | -- | Capability to obtain absolute time, namely via the `time` command.
    CTimeabs
  | -- | Capability to utilize relative passage of time, namely via the `wait` command.
    --   This is strictly weaker than "CTimeAbs".
    CTimerel
  | -- | Capability to execute `try`.
    CTry
  | -- | Capability for working with sum types.
    CSum
  | -- | Capability for working with product types.
    CProd
  | -- | Capability for working with record types.
    CRecord
  | -- | Debug capability.
    CDebug
  | -- | Capability to handle keyboard input.
    CHandleinput
  | -- | Capability to make other robots halt.
    CHalt
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
      Nothing -> failT ["Unknown capability", t]

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
  Equipped -> Nothing
  -- speaking is natural to robots (unlike listening)
  Say -> Nothing
  -- TODO: #495
  --   the require command will be inlined once the Issue is fixed
  --   so the capabilities of the run commands will be checked instead
  Run -> Nothing
  -- ----------------------------------------------------------------
  -- Some straightforward ones.
  Listen -> Just CListen
  Log -> Just CLog
  Selfdestruct -> Just CSelfdestruct
  Move -> Just CMove
  Push -> Just CPush
  Stride -> Just CMovemultiple
  Turn -> Just CTurn
  Grab -> Just CGrab
  Harvest -> Just CHarvest
  Place -> Just CPlace
  Give -> Just CGive
  Equip -> Just CEquip
  Unequip -> Just CUnequip
  Make -> Just CMake
  Count -> Just CCount
  If -> Just CCond
  Blocked -> Just CSensefront
  Scan -> Just CScan
  Ishere -> Just CSensehere
  Isempty -> Just CSensehere
  Upload -> Just CScan
  Build -> Just CBuild
  Salvage -> Just CSalvage
  Reprogram -> Just CReprogram
  Meet -> Just CMeet
  MeetAll -> Just CMeet
  Act -> Just CAct
  Drill -> Just CDrill
  Use -> Nothing -- Recipes alone shall dictate whether things can be "used"
  Neg -> Just CArith
  Add -> Just CArith
  Sub -> Just CArith
  Mul -> Just CArith
  Div -> Just CArith
  Exp -> Just CArith
  Whoami -> Just CWhoami
  Self -> Just CWhoami
  Swap -> Just CSwap
  Atomic -> Just CAtomic
  Instant -> Just CGod
  Time -> Just CTimeabs
  Wait -> Just CTimerel
  Scout -> Just CRecondir
  Whereami -> Just CSenseloc
  Detect -> Just CDetectloc
  Resonate -> Just CDetectcount
  Sniff -> Just CDetectdistance
  Chirp -> Just CDetectdirection
  Watch -> Just CWakeself
  Heading -> Just COrient
  Key -> Just CHandleinput
  InstallKeyHandler -> Just CHandleinput
  Halt -> Just CHalt
  -- ----------------------------------------------------------------
  -- Text operations
  Format -> Just CText
  Concat -> Just CText
  Split -> Just CText
  Chars -> Just CText
  CharAt -> Just CCode
  ToChar -> Just CCode
  -- ----------------------------------------------------------------
  -- Some God-like abilities.
  As -> Just CGod
  RobotNamed -> Just CGod
  RobotNumbered -> Just CGod
  Create -> Just CGod
  Surveil -> Just CGod
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
  -- exceptions
  Try -> Just CTry
  -- ----------------------------------------------------------------
  -- type-level arithmetic
  Inl -> Just CSum
  Inr -> Just CSum
  Case -> Just CSum
  Fst -> Just CProd
  Snd -> Just CProd
  -- TODO: #563 pair syntax (1,2,3...) should require CProd too

  -- ----------------------------------------------------------------
  -- Some additional straightforward ones, which however currently
  -- cannot be used in classic mode since there is no craftable item
  -- which conveys their capability. TODO: #26
  Teleport -> Just CTeleport -- Some space-time machine like Tardis?
  Appear -> Just CAppear -- paint?
  Random -> Just CRandom -- randomness device (with bitcoins)?
  -- ----------------------------------------------------------------
  -- Some more constants which *ought* to have their own capability but
  -- currently don't.
  View -> Nothing -- TODO: #17 should require equipping an antenna
  Knows -> Nothing
