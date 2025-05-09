-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A quasiquoter for Swarm polytypes + untyped terms.
module Swarm.Language.Parser.QQ (tyQ, astQ) where

import Data.Generics
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Swarm.Language.Parser.Core (runParserTH)
import Swarm.Language.Parser.Lex (sc)
import Swarm.Language.Parser.Term (parseTerm)
import Swarm.Language.Parser.Type (parsePolytype)
import Swarm.Language.Parser.Util (fully)
import Swarm.Language.Syntax
import Swarm.Util (liftText)
import Witch (from)

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
    -- Using `error` is OK here since a quasiquoter will only ever run
    -- at compile time; hence it can only make compilation fail, not
    -- crash the game at runtime.
    , quotePat = error "quotePat  not implemented for polytypes"
    , quoteType = error "quoteType not implemented for polytypes"
    , quoteDec = error "quoteDec  not implemented for polytypes"
    }

quoteTypeExp :: String -> TH.ExpQ
quoteTypeExp s = do
  loc <- TH.location
  parsed <- runParserTH loc (fully sc parsePolytype) s
  dataToExpQ (fmap liftText . cast) parsed

astQ :: QuasiQuoter
astQ =
  QuasiQuoter
    { quoteExp = quoteASTExp
    , quotePat = error "quotePat  not implemented for ASTs"
    , quoteType = error "quoteType not implemented for ASTs"
    , quoteDec = error "quoteDec  not implemented for ASTs"
    }

quoteASTExp :: String -> TH.ExpQ
quoteASTExp s = do
  loc <- TH.location
  parsed <- runParserTH loc (fully sc parseTerm) s
  dataToExpQ ((fmap liftText . cast) `extQ` antiASTExp) parsed

antiASTExp :: Syntax -> Maybe TH.ExpQ
antiASTExp (STerm (TAntiSyn v)) = Just $ TH.varE (TH.mkName (from v))
antiASTExp _ = Nothing
