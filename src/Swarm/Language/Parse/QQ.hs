-- |
-- Module      :  Swarm.Language.Parse.QQ
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A quasiquoter for Swarm polytypes.
module Swarm.Language.Parse.QQ (tyQ) where

import Data.Generics
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote

import Swarm.Language.Parse
import Swarm.Util (liftText)

------------------------------------------------------------
-- Quasiquoters
------------------------------------------------------------

-- | A quasiquoter for Swarm polytypes, so we can conveniently write them
--   down using concrete syntax and have them parsed into abstract
--   syntax at compile time.  This is used, for example, in writing down
--   the concrete types of constants (see "Swarm.Language.Typecheck").
tyQ :: QuasiQuoter
tyQ =
  QuasiQuoter
    { quoteExp = quoteTypeExp
    , quotePat = error "quotePat  not implemented for polytypes"
    , quoteType = error "quoteType not implemented for polytypes"
    , quoteDec = error "quoteDec  not implemented for polytypes"
    }

quoteTypeExp :: String -> TH.ExpQ
quoteTypeExp s = do
  loc <- TH.location
  let pos =
        ( TH.loc_filename loc
        , fst (TH.loc_start loc)
        , snd (TH.loc_start loc)
        )
  parsed <- runParserTH pos parsePolytype s
  dataToExpQ (fmap liftText . cast) parsed
