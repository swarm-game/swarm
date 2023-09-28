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
  allCapabilities,
) where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Language.Syntax
import Swarm.Util (failT)
import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (mapMaybe)

-- | Various capabilities which robots can have.
data Capability
  = -- | Execute the command or function.
    CExecute Const
  | -- | Be powered, i.e. execute anything at all
    CPower
  | -- | Convert between characters/text and Unicode values
    CCode
  | -- | Allow a heavy robot to move, backup and stride.
    CMoveHeavy
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
  | -- | Capability to introspect and see its own name
    CWhoami
  | -- | Capability for working with sum types.
    CSum
  | -- | Capability for working with product types.
    CProd
  | -- | Capability for working with record types.
    CRecord
  | -- | Debug capability.
    CDebug
  | -- | Capability to handle keyboard input.
    CHandleInput
  | -- | God-like capabilities.  For e.g. commands intended only for
    --   checking challenge mode win conditions, and not for use by
    --   players.
    CGod
  deriving (Eq, Ord, Show, Generic, Hashable, Data, FromJSONKey, ToJSONKey)

allCapabilities :: [Capability]
allCapabilities = Map.keys $ fst capabilityNames'

capabilityNames :: (Capability -> Text, Text -> Maybe Capability)
capabilityNames = ((fromC Map.!), (`Map.lookup` toC))
  where
    (fromC, toC) = capabilityNames'

capabilityNames' :: (Map Capability Text, Map Text Capability)
capabilityNames' = (fromC, toC)
  where
    toC = Map.fromList mapping
    fromC = Map.fromList $ map (\(t, c) -> (c, t)) mapping
    mapping = mappingExec <> mapping'
    mappingExec = map (\c -> (T.toLower . T.pack $ show c, CExecute c)) $ Set.toList execCaps
    mapping' =
      [ ("power", CPower)
      , ("code", CCode)
      , ("float", CFloat)
      , ("cond", CCond)
      , ("negation", CNegation)
      , ("compare", CCompare)
      , ("orient", COrient)
      , ("arith", CArith)
      , ("env", CEnv)
      , ("lambda", CLambda)
      , ("recursion", CRecursion)
      , ("whoami", CWhoami)
      , ("sum", CSum)
      , ("prod", CProd)
      , ("record", CRecord)
      , ("debug", CDebug)
      , ("heavy robot move", CMoveHeavy)
      , ("handle input", CHandleInput)
      , ("god", CGod)]

-- TODO: test no show instance
capabilityName :: Capability -> Text
capabilityName = fst capabilityNames

instance ToJSON Capability where
  toJSON = String . capabilityName

instance FromJSON Capability where
  parseJSON = withText "Capability" tryRead
   where
    tryRead :: Text -> Parser Capability
    tryRead t = case snd capabilityNames $ T.strip t of
        Just c -> return c
        Nothing -> failT ["Unknown capability", t]

execCaps :: Set Const
execCaps = Set.fromList . mapMaybe getConst $ mapMaybe constCaps allConst
  where
    getConst = \case
      CExecute c -> Just c
      _ -> Nothing

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
  -- Recipes alone shall dictate whether things can be "used"
  Use -> Nothing
  -- ----------------------------------------------------------------
  Neg -> Just CArith
  Add -> Just CArith
  Sub -> Just CArith
  Mul -> Just CArith
  Div -> Just CArith
  Exp -> Just CArith
  Whoami -> Just CWhoami
  Self -> Just CWhoami
  Heading -> Just COrient
  Key -> Just CHandleInput
  InstallKeyHandler -> Just CHandleInput
  -- ----------------------------------------------------------------
  -- Some God-like abilities.
  As -> Just CGod
  RobotNamed -> Just CGod
  RobotNumbered -> Just CGod
  Create -> Just CGod
  Surveil -> Just CGod
  Instant -> Just CGod
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
  -- type-level arithmetic
  Inl -> Just CSum
  Inr -> Just CSum
  Case -> Just CSum
  Fst -> Just CProd
  Snd -> Just CProd
  -- TODO: #563 pair syntax (1,2,3...) should require CProd too
  -- ----------------------------------------------------------------
  -- Some more constants which *ought* to have their own capability but
  -- currently don't.
  View -> Nothing -- TODO: #17 should require equipping an antenna
  Knows -> Nothing
  -- ----------------------------------------------------------------
  -- The rest is straightforward
  {- TODO: #26
  Some capabilities currently cannot be used in classic mode since there
  is no craftable item which conveys their capability:
  * Teleport  Some space-time machine like Tardis?
  * Appear  paint?
  * Random  randomness device (with bitcoins)?
  -}
  c -> Just (CExecute c)
