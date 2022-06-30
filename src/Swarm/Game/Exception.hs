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

  -- * Helper functions
  formatIncapable,
  formatIncapableFix,
) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T

import Control.Lens ((^.))
import Data.Set qualified as S
import Swarm.Game.Entity (EntityMap, deviceForCap, entityName)
import Swarm.Language.Capability (Capability (CGod), capabilityName)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax (Const, Term)
import Swarm.Util

-- ------------------------------------------------------------------
-- SETUP FOR DOCTEST

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Set qualified as S
-- >>> import Data.Text (unpack)
-- >>> import Swarm.Language.Syntax
-- >>> import Swarm.Language.Capability
-- >>> import Swarm.Game.Entity
-- >>> import Swarm.Game.Display

-- ------------------------------------------------------------------

-- | Suggested way to fix incapable error.
data IncapableFix
  = -- | Install the missing device on yourself/target
    FixByInstall
  | -- | Add the missing device to your inventory
    FixByObtain
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
  | -- | The user program explicitly called 'Undefined' or 'Fail'.
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
  FixByInstall -> "install"
  FixByObtain -> "obtain"

-- | Pretty print the incapable exception with an actionable suggestion
--   on how to fix it.
--
-- >>> w = mkEntity (defaultEntityDisplay 'l') "magic wand" [] [] [CAppear]
-- >>> r = mkEntity (defaultEntityDisplay 'o') "the one ring" [] [] [CAppear]
-- >>> m = buildEntityMap [w,r]
-- >>> incapableError cs t = putStr . unpack $ formatIncapable m FixByInstall cs t
--
-- >>> incapableError (S.singleton CGod) (TConst As)
-- Thou shalt not utter such blasphemy:
--   'as'
--   If God in troth thou wantest to play, try thou a Creative game.
--
-- >>> incapableError (S.singleton CAppear) (TConst Appear)
-- You do not have the devices required for:
--   'appear'
--   please install:
--    - the one ring or magic wand
--
-- >>> incapableError (S.singleton CRandom) (TConst Random)
-- Missing the random capability for:
--   'random'
--   but no device yet provides it. See
--   https://github.com/swarm-game/swarm/issues/26
formatIncapable :: EntityMap -> IncapableFix -> Set Capability -> Term -> Text
formatIncapable em f caps tm
  | CGod `S.member` caps =
    unlinesExText
      [ "Thou shalt not utter such blasphemy:"
      , squote $ prettyText tm
      , "If God in troth thou wantest to play, try thou a Creative game."
      ]
  | not (null capsNone) =
    unlinesExText
      [ "Missing the " <> capMsg <> " for:"
      , squote $ prettyText tm
      , "but no device yet provides it. See"
      , "https://github.com/swarm-game/swarm/issues/26"
      ]
  | otherwise =
    unlinesExText
      ( "You do not have the devices required for:" :
        squote (prettyText tm) :
        "please " <> formatIncapableFix f <> ":" :
        ((" - " <>) . formatDevices <$> filter (not . null) deviceSets)
      )
 where
  capList = S.toList caps
  deviceSets = map (`deviceForCap` em) capList
  devicePerCap = zip capList deviceSets
  -- capabilities not provided by any device
  capsNone = map (capabilityName . fst) $ filter (null . snd) devicePerCap
  capMsg = case capsNone of
    [ca] -> ca <> " capability"
    cas -> "capabilities " <> T.intercalate ", " cas
  formatDevices = T.intercalate " or " . map (^. entityName)

-- | Exceptions that span multiple lines should be indented.
unlinesExText :: [Text] -> Text
unlinesExText ts = T.unlines . (head ts :) . map ("  " <>) $ tail ts
