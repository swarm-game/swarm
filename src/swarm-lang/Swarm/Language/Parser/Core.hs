{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Core data type definitions and utilities for the Swarm language
-- parser.
module Swarm.Language.Parser.Core (
  -- * Antiquoting
  Antiquoting (..),

  -- * Comment parsing state
  CommentState (..),
  freshLine,
  comments,

  -- * Parser type
  Parser,
  ParserError,

  -- ** Running
  runParser,
  runParserTH,
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Data.Bifunctor (first, second)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Void (Void)
import Swarm.Language.Syntax (Comment)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.State (initialPosState, initialState)
import Witch (from)

------------------------------------------------------------
-- Custom parser state

-- | When parsing a term using a quasiquoter (i.e. something in the
--   Swarm source code that will be parsed at compile time), we want
--   to allow antiquoting, i.e. writing something like $x to refer to
--   an existing Haskell variable.  But when parsing a term entered by
--   the user at the REPL, we do not want to allow this syntax.
data Antiquoting = AllowAntiquoting | DisallowAntiquoting
  deriving (Eq, Ord, Show)

data CommentState = CS
  { _freshLine :: Bool
  -- ^ Are we currently on a (so far) blank line, i.e. have there been
  --   no nontrivial tokens since the most recent newline?  This field
  --   is updated every time we parse a lexeme or symbol (set to
  --   false), or a newline (set to true).
  , _comments :: Seq Comment
  -- ^ The actual sequence of comments, in the order they were encountered
  }

makeLenses ''CommentState

initCommentState :: CommentState
initCommentState = CS {_freshLine = True, _comments = Seq.empty}

------------------------------------------------------------
-- Parser types

type Parser = ReaderT Antiquoting (StateT CommentState (Parsec Void Text))

type ParserError = ParseErrorBundle Text Void

------------------------------------------------------------
-- Running parsers

-- | Run a parser on some input text, returning either the result +
--   all collected comments, or a parse error message.
runParser :: Parser a -> Text -> Either ParserError (a, Seq Comment)
runParser p t =
  (\pt -> parse pt "" t)
    . fmap (second (^. comments))
    . flip runStateT initCommentState
    . flip runReaderT DisallowAntiquoting
    $ p

-- | A utility for running a parser in an arbitrary 'MonadFail' (which
--   is going to be the TemplateHaskell 'Language.Haskell.TH.Q' monad --- see
--   "Swarm.Language.Parser.QQ"), with a specified source position.
runParserTH :: (Monad m, MonadFail m) => (String, Int, Int) -> Parser a -> String -> m a
runParserTH (file, line, col) p s =
  let (_, res) =
        flip runParser' initState
          . flip runStateT initCommentState
          . flip runReaderT AllowAntiquoting
          $ p
   in case res of
        Left err -> fail $ errorBundlePretty err
        Right e -> return (fst e)
 where
  initState :: State Text Void
  initState =
    (initialState file (from s))
      { statePosState =
          (initialPosState file (from s))
            { pstateSourcePos = SourcePos file (mkPos line) (mkPos col)
            }
      }
