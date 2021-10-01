-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Swarm.Language.Pipeline.QQ
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A quasiquoter for Swarm terms.
module Swarm.Language.Pipeline.QQ (tmQ) where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Witch (from)

import Swarm.Language.Parse
import Swarm.Language.Pipeline
import Swarm.Language.Syntax
import Swarm.Util (liftText)

-- | A quasiquoter for Swarm language terms, so we can conveniently
--   write them down using concrete syntax and have them parsed into
--   abstract syntax at compile time.  The quasiquoter actually runs
--   the entire pipeline on them (parsing, typechecking, elaborating),
--   so a quasiquoted Swarm program with a parse error or a type error
--   will fail at Haskell compile time.  This is useful for creating
--   system robot programs (for example, see
--   'Swarm.Game.Step.seedProgram').
tmQ :: QuasiQuoter
tmQ =
  QuasiQuoter
    { quoteExp = quoteTermExp
    , quotePat = error "quotePat  not implemented for terms"
    , quoteType = error "quoteType not implemented for terms"
    , quoteDec = error "quoteDec  not implemented for terms"
    }

quoteTermExp :: String -> TH.ExpQ
quoteTermExp s = do
  loc <- TH.location
  let pos =
        ( TH.loc_filename loc
        , fst (TH.loc_start loc)
        , snd (TH.loc_start loc)
        )
  parsed <- runParserTH pos parseTerm s
  case processParsedTerm parsed of
    Left errMsg -> fail $ from errMsg
    Right ptm -> dataToExpQ ((fmap liftText . cast) `extQ` antiTermExp) ptm

antiTermExp :: Term -> Maybe TH.ExpQ
antiTermExp (TAntiString v) =
  Just $ TH.appE (TH.conE (TH.mkName "TString")) (TH.varE (TH.mkName (from v)))
antiTermExp (TAntiInt v) =
  Just $ TH.appE (TH.conE (TH.mkName "TInt")) (TH.varE (TH.mkName (from v)))
antiTermExp _ = Nothing

-- At the moment, only antiquotation of literal strings and ints are
-- supported, because that's what we need for the seedProgram.  But
-- we can easily add more in the future.
