{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.Game.Exception
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Runtime exceptions for the Swarm language interpreter.
module Swarm.Game.Exception (
  Exn (..),
  IncapableFix (..),
  formatExn,
) where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens ((^.))
import qualified Data.Set as S
import Swarm.Game.Entity (EntityMap, deviceForCap, entityName)
import Swarm.Language.Capability
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Util

-- | Suggested way to fix incapable error.
data IncapableFix
  = FixByInstall -- ^ install the missing device on yourself/target
  | FixByObtain -- ^ add the missing device to your inventory
  deriving (Eq, Show)


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
    --   @try@ block.
    Incapable IncapableFix (Set Capability) Term
  | -- | A command failed in some "normal" way (/e.g./ a 'Move'
    --   command could not move, or a 'Grab' command found nothing to
    --   grab, /etc./).
    CmdFailed Const Text
  | -- | The user program explicitly called 'Raise', 'Undefined', or 'ErrorStr'.
    User Text
  deriving (Eq, Show)

-- | Pretty-print an exception for displaying to the player.
formatExn :: EntityMap -> Exn -> Text
formatExn em = \case
  Fatal t ->
    T.unlines
      [ "Fatal error: " <> t
      , "Please report this as a bug at"
      , "<https://github.com/swarm-game/swarm/issues/new>."
      ]
  InfiniteLoop -> "Infinite loop detected!"
  (CmdFailed c t) -> T.concat [prettyText c, ": ", t]
  (User t) -> "Player exception: " <> t
  (Incapable f caps tm) -> formatIncapable em f caps tm

-- ------------------------------------------------------------------
-- INCAPABLE HELPERS
-- ------------------------------------------------------------------

formatIncapableFix :: IncapableFix -> Text
formatIncapableFix = \case
  FixByInstall -> "Install"
  FixByObtain -> "Obtain"

-- | Pretty print the incapable exception with actionable suggestion
--   on what to install to fix it.
formatIncapable :: EntityMap -> IncapableFix -> Set Capability -> Term -> Text
formatIncapable em f caps tm
  | CGod `S.member` caps = "Can not perform an impossible task:\n" <> prettyText tm
  | not (null capsNone) =
      T.unlines
        [ "Missing the " <> capMsg <> " required for:"
        , prettyText tm
        , "because no device can provide it."
        , "See https://github.com/swarm-game/swarm/issues/26."
        ]
  | otherwise =
      T.unlines
        ( "You do not have the devices required for:" :
          prettyText tm :
          formatIncapableFix f <>":" :
          (("  - " <>) . formatDevices <$> filter (not . null) deviceSets)
        )
 where
  capList = S.toList caps
  deviceSets = map (`deviceForCap` em) capList
  devicePerCap = zip capList deviceSets
  -- capabilities not provided by any device
  capsNone = map (capabilityName . fst) $ filter (null . snd) devicePerCap
  capMsg = case capsNone of
    [ca] -> ca <> " capability"
    cas -> "capabilities " <> T.intercalate ", " (map squote cas)
  formatDevices = T.intercalate " or " . map (^. entityName)
