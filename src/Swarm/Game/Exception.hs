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
  formatExn,
) where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Swarm.Language.Capability
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Swarm.Util

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
    Incapable (Set Capability) Term
  | -- | A command failed in some "normal" way (/e.g./ a 'Move'
    --   command could not move, or a 'Grab' command found nothing to
    --   grab, /etc./).
    CmdFailed Const Text
  | -- | The user program explicitly called 'Raise'.
    User Text
  deriving (Eq, Show)

-- | Pretty-print an exception for displaying to the user.
formatExn :: Exn -> Text
formatExn (Fatal t) =
  T.unlines
    [ T.append "fatal error: " t
    , "Please report this as a bug at https://github.com/byorgey/swarm/issues/new ."
    ]
formatExn InfiniteLoop = "Infinite loop detected!"
formatExn (Incapable _caps tm) =
  T.concat
    [ "missing device(s) needed to execute command "
    , squote (prettyText tm)
    ]
formatExn (CmdFailed c t) = T.concat [prettyText c, ": ", t]
formatExn (User t) = T.concat ["user exception: ", t]
