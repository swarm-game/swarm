-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Parse.QQ
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Quasiquoters for Swarm types and terms, so we can conveniently
-- write them down using concrete syntax and have them parsed into
-- abstract syntax at compile time.  This is used, for example, in
-- writing down the concrete types of constants (see
-- "Swarm.Language.Typecheck"), and in creating system robot programs
-- (see 'Swarm.Game.Step.seedProgram').
--
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Language.Parse.QQ
  ( tyQ
  , tmQ
  )
  where

import           Data.Generics
import qualified Data.Text                  as T
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Witch                      (from)

import           Swarm.Language.Parse
import           Swarm.Language.Syntax

------------------------------------------------------------
-- Parsing utilities
------------------------------------------------------------

-- See https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

------------------------------------------------------------
-- Quasiquoters
------------------------------------------------------------

-- | A quasiquoter for Swarm polytypes.
tyQ :: QuasiQuoter
tyQ = QuasiQuoter
  { quoteExp  = quoteTypeExp
  , quotePat  = error "quotePat  not implemented for polytypes"
  , quoteType = error "quoteType not implemented for polytypes"
  , quoteDec  = error "quoteDec  not implemented for polytypes"
  }

quoteTypeExp :: String -> TH.ExpQ
quoteTypeExp s = do
  loc <- TH.location
  let pos =
        (TH.loc_filename loc,
         fst (TH.loc_start loc),
         snd (TH.loc_start loc))
  parsed <- runParserTH pos parsePolytype s
  dataToExpQ (fmap liftText . cast) parsed

-- | A quasiquoter for Swarm language terms.
tmQ :: QuasiQuoter
tmQ = QuasiQuoter
  { quoteExp = quoteTermExp
  , quotePat  = error "quotePat  not implemented for terms"
  , quoteType = error "quoteType not implemented for terms"
  , quoteDec  = error "quoteDec  not implemented for terms"
  }

quoteTermExp :: String -> TH.ExpQ
quoteTermExp s = do
  loc <- TH.location
  let pos =
        (TH.loc_filename loc,
         fst (TH.loc_start loc),
         snd (TH.loc_start loc))
  parsed <- runParserTH pos parseTerm s
  dataToExpQ ((fmap liftText . cast) `extQ` antiTermExp) parsed

antiTermExp :: Term -> Maybe TH.ExpQ
antiTermExp (TAntiString v) =
  Just $ TH.appE (TH.conE (TH.mkName "TString")) (TH.varE (TH.mkName (from v)))
antiTermExp _ = Nothing
  -- At the moment, only antiquotation of literal strings is
  -- supported, because that's what we needed for the seedProgram.
  -- But we can easily add more in the future.
