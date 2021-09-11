-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.CEK
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Runtime exceptions for the Swarm language interpreter.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Exception where

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Swarm.Language.Pretty (prettyText)
import           Swarm.Language.Syntax

data Exn
    -- | Something went very wrong.  This is a bug in Swarm and cannot
    --   be caught by a @try@ block (but at least it will not crash
    --   the entire UI).
  = Fatal Text

    -- | A command failed in some way (/e.g./ a 'Move' command could
    --   not move, or a 'Grab' command found nothing to grab, /etc./).
  | CmdFailed Const Text

    -- | The user program explicitly called 'Raise'.
  | User Text
  deriving (Eq, Show)

formatExn :: Exn -> Text
formatExn (Fatal t) = T.unlines
  [ T.append "fatal error: " t
  , "Please report this as a bug at https://github.com/byorgey/swarm/issues ."
  ]
formatExn (CmdFailed c t) = T.concat [prettyText c, ": ", t]
formatExn (User t)        = T.concat ["user exception: ", t]
