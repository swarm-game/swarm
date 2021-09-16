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

module Swarm.Game.Exception
  ( Exn(..)
  , formatExn
  )
  where

import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Swarm.Language.Capability
import           Swarm.Language.Pretty     (prettyText)
import           Swarm.Language.Syntax
import           Swarm.Util

-- | The type of exceptions that can be thrown by robot programs.
data Exn
    -- | Something went very wrong.  This is a bug in Swarm and cannot
    --   be caught by a @try@ block (but at least it will not crash
    --   the entire UI).
  = Fatal Text

    -- | A robot tried to do something for which it does not have some
    --   of the required capabilities.  This cannot be caught by a
    --   @try@ block.
  | Incapable (Set Capability) Term

    -- | A command failed in some "normal" way (/e.g./ a 'Move'
    --   command could not move, or a 'Grab' command found nothing to
    --   grab, /etc./).
  | CmdFailed Const Text

    -- | The user program explicitly called 'Raise'.
  | User Text
  deriving (Eq, Show)

-- | Pretty-print an exception for displaying to the user.
formatExn :: Exn -> Text
formatExn (Fatal t) = T.unlines
  [ T.append "fatal error: " t
  , "Please report this as a bug at https://github.com/byorgey/swarm/issues/new ."
  ]
formatExn (Incapable caps tm)   = T.unlines
  [ T.concat
    [ "missing ", number (S.size caps) "capability", " "
    , (commaList . map (squote . prettyText) .  S.toList) caps, " needed to execute:"
    ]
  , prettyText tm
  ]
formatExn (CmdFailed c t) = T.concat [prettyText c, ": ", t]
formatExn (User t)        = T.concat ["user exception: ", t]
