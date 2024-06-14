{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Runtime exceptions for the Swarm language interpreter.
module Swarm.Game.Exception (
  Exn (..),
  IncapableFix (..),
  formatExn,
  IncapableFixWords (..),

  -- * Helper functions
  formatIncapable,
  formatIncapableFix,
) where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Constant
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Entity (EntityMap, devicesForCap, entityName)
import Swarm.Language.Capability (Capability (CGod), capabilityName)
import Swarm.Language.JSON ()
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Requirements.Type (Requirements (..))
import Swarm.Language.Syntax (Const, Term)
import Swarm.Util
import Witch (from)

-- ------------------------------------------------------------------
-- SETUP FOR DOCTEST

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Text (unpack)
-- >>> import Swarm.Language.Syntax
-- >>> import Swarm.Language.Capability
-- >>> import Swarm.Game.Entity
-- >>> import Swarm.Game.Display
-- >>> import qualified Swarm.Language.Requirement as R

-- ------------------------------------------------------------------

-- | Suggested way to fix things when a robot does not meet the
--   requirements to run a command.
data IncapableFix
  = -- | 'Swarm.Language.Syntax.Equip' the missing device on yourself/target
    FixByEquip
  | -- | Add the missing device to your inventory
    FixByObtainDevice
  | -- | Add the missing consumables to your inventory
    FixByObtainConsumables
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | The type of exceptions that can be thrown by robot programs.
data Exn
  = -- | Something went very wrong.  This is a bug in Swarm and cannot
    --   be caught by a @try@ block (but at least it will not crash
    --   the entire UI).
    Fatal Text
  | -- | An infinite loop was detected via a blackhole.  This cannot
    --   be caught by a @try@ block.
    InfiniteLoop
  | -- | A robot tried to do something for which it does not have some
    --   of the required capabilities.  This cannot be caught by a
    --   @try@ block.  Also contains the missing requirements, the
    --   term that caused the problem, and a suggestion for how to fix
    --   things.
    Incapable IncapableFix Requirements Term
  | -- | A command failed in some "normal" way (/e.g./ a 'Swarm.Language.Syntax.Move'
    --   command could not move, or a 'Swarm.Language.Syntax.Grab' command found nothing to
    --   grab, /etc./).  Can be caught by a @try@ block.
    CmdFailed Const Text (Maybe GameplayAchievement)
  | -- | The user program explicitly called 'Swarm.Language.Syntax.Undefined' or 'Swarm.Language.Syntax.Fail'. Can
    --   be caught by a @try@ block.
    User Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Pretty-print an exception for displaying to the player.
formatExn :: EntityMap -> Exn -> Text
formatExn em = \case
  Fatal t ->
    T.unlines
      [ "Fatal error: " <> t
      , "Please report this as a bug at"
      , "<" <> swarmRepoUrl <> "issues/new>."
      ]
  InfiniteLoop -> "Infinite loop detected!"
  (CmdFailed c t _) -> T.concat [prettyText c, ": ", t]
  (User t) -> "Player exception: " <> t
  (Incapable f caps tm) -> formatIncapable em f caps tm

-- ------------------------------------------------------------------
-- INCAPABLE HELPERS
-- ------------------------------------------------------------------

data IncapableFixWords = IncapableFixWords
  { fixVerb :: Text
  , fixNoun :: Text
  }

-- | Pretty-print an 'IncapableFix': either "equip device",
-- "obtain device", or "obtain consumables".
formatIncapableFix :: IncapableFix -> IncapableFixWords
formatIncapableFix = \case
  FixByEquip -> IncapableFixWords "equip" "device"
  FixByObtainDevice -> IncapableFixWords "obtain" "device"
  FixByObtainConsumables -> IncapableFixWords "obtain" "consumables"

-- | Pretty print the incapable exception with an actionable suggestion
--   on how to fix it.
--
-- >>> import Data.Either (fromRight)
-- >>> import Control.Carrier.Throw.Either (runThrow)
-- >>> import Control.Algebra (run)
-- >>> import Swarm.Game.Failure (LoadingFailure)
-- >>> import qualified Data.Set as S
-- >>> :set -XTypeApplications
-- >>> w = mkEntity (defaultEntityDisplay 'l') "magic wand" mempty mempty (S.singleton CAppear)
-- >>> r = mkEntity (defaultEntityDisplay 'o') "the one ring" mempty mempty (S.singleton CAppear)
-- >>> m = fromRight mempty . run . runThrow @LoadingFailure $ buildEntityMap [w,r]
-- >>> incapableError cs t = putStr . unpack $ formatIncapable m FixByEquip cs t
--
-- >>> incapableError (R.singletonCap CGod) (TConst As)
-- Thou shalt not utter such blasphemy:
--   'as'
--   If God in troth thou wantest to play, try thou a Creative game.
--
-- >>> incapableError (R.singletonCap CAppear) (TConst Appear)
-- You do not have the device required for:
--   'appear'
--   Please equip:
--   - magic wand or the one ring
--
-- >>> incapableError (R.singletonCap CRandom) (TConst Random)
-- Missing the random capability for:
--   'random'
--   but no device yet provides it. See
--   https://github.com/swarm-game/swarm/issues/26
--
-- >>> incapableError (R.singletonInv 3 "tree") (TConst Noop)
-- You are missing required inventory for:
--   'noop'
--   Please obtain:
--   - tree (3)
formatIncapable :: EntityMap -> IncapableFix -> Requirements -> Term -> Text
formatIncapable em f (Requirements caps _ inv) tm
  | CGod `S.member` caps =
      unlinesExText $
        "Thou shalt not utter such blasphemy:"
          :| [ squote $ prettyText tm
             , "If God in troth thou wantest to play, try thou a Creative game."
             ]
  | not (null capsNone) =
      unlinesExText $
        "Missing the " <> capMsg <> " for:"
          :| [ squote $ prettyText tm
             , "but no device yet provides it. See"
             , swarmRepoUrl <> "issues/26"
             ]
  | not (S.null caps) =
      let IncapableFixWords fVerb fNoun = formatIncapableFix f
       in unlinesExText
            ( T.unwords ["You do not have the", fNoun, "required for:"]
                :| squote (prettyText tm)
                : "Please " <> fVerb <> ":"
                : (("- " <>) . formatDevices <$> filter (not . null) deviceSets)
            )
  | otherwise =
      unlinesExText
        ( "You are missing required inventory for:"
            :| squote (prettyText tm)
            : "Please obtain:"
            : (("- " <>) . formatEntity <$> M.assocs inv)
        )
 where
  capList = S.toList caps
  deviceSets = map (`devicesForCap` em) capList
  devicePerCap = zip capList deviceSets
  -- capabilities not provided by any device
  capsNone = map (capabilityName . fst) $ filter (null . snd) devicePerCap
  capMsg = case capsNone of
    [ca] -> ca <> " capability"
    cas -> "capabilities " <> T.intercalate ", " cas
  formatDevices = T.intercalate " or " . map (^. entityName)
  formatEntity (e, 1) = e
  formatEntity (e, n) = e <> " (" <> from (show n) <> ")"

-- | Exceptions that span multiple lines should be indented.
unlinesExText :: NonEmpty Text -> Text
unlinesExText (t :| ts) = T.unlines $ (t :) $ map ("  " <>) ts
