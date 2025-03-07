{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A quasiquoter for Swarm terms.
module Swarm.Language.Pipeline.QQ (tmQ) where

import Data.Generics
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote
import Swarm.Language.Parser.Core (runParserTH)
import Swarm.Language.Parser.Lex (sc)
import Swarm.Language.Parser.Term (parseTerm)
import Swarm.Language.Parser.Util (fully)
import Swarm.Language.Pipeline
import Swarm.Language.Syntax
import Swarm.Language.Types (Polytype)
import Swarm.Pretty (prettyText)
import Swarm.Util (failT, liftText)
import Witch (from, into)

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
quoteTermExp (into @Text -> s) = do
  loc <- TH.location
  parsed <- runParserTH loc (fully sc parseTerm) s
  processed <- TH.runIO $ processParsedTerm (s, parsed)
  case processed of
    Left err -> failT [prettyText err]
    Right ptm -> dataToExpQ ((fmap liftText . cast) `extQ` antiTermExp) ptm

antiTermExp :: Term' Polytype -> Maybe TH.ExpQ
antiTermExp (TAntiText v) =
  Just $ TH.appE (TH.conE (TH.mkName "TText")) (TH.varE (TH.mkName (from v)))
antiTermExp (TAntiInt v) =
  Just $ TH.appE (TH.conE (TH.mkName "TInt")) (TH.varE (TH.mkName (from v)))
antiTermExp _ = Nothing

-- At the moment, only antiquotation of literal text and ints are
-- supported, because that's what we need for the seedProgram.  But
-- we can easily add more in the future.
