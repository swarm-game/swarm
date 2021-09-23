-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Parse.QQ
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A quasiquoter for Swarm types, so we can conveniently write down
-- types using concrete syntax and have them parsed into abstract
-- syntax at compile time.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Swarm.Language.Parse.QQ
  ( tyQ
  )
  where

import           Data.Data                  (cast)
import qualified Data.Text                  as T
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Swarm.Language.Parse

-- | A quasiquoter for Swarm polytypes.
tyQ :: QuasiQuoter
tyQ = QuasiQuoter
  { quoteExp  = quoteTypeExp
  , quotePat  = error "quotePat  not implemented for polytypes"
  , quoteType = error "quoteType not implemented for polytypes"
  , quoteDec  = error "quoteDec  not implemented for polytypes"
  }

-- See https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

quoteTypeExp :: String -> TH.ExpQ
quoteTypeExp s = do
  loc <- TH.location
  let pos =
        (TH.loc_filename loc,
         fst (TH.loc_start loc),
         snd (TH.loc_start loc))
  typeExpr <- runParserTH pos parsePolytype s
  dataToExpQ (fmap liftText . cast) typeExpr
