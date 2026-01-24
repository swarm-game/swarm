-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A few utilities for use in conjunction with the parser.
module Swarm.Language.Parser.Util (
  fully,
  fullyMaybe,
  showShortError,
  showErrorPos,
  getLocRange,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Parser.Core (ParserError)
import Text.Megaparsec
import Text.Megaparsec.Pos qualified as Pos
import Witch (from)

-- | Run a parser "fully", consuming leading whitespace and ensuring
--   that the parser extends all the way to eof.
fully :: (MonadParsec e s f) => f () -> f a -> f a
fully sc p = sc *> p <* eof

-- | Run a parser "fully", consuming leading whitespace (including the
--   possibility that the input is nothing but whitespace) and
--   ensuring that the parser extends all the way to eof.
fullyMaybe :: (MonadParsec e s f) => f () -> f a -> f (Maybe a)
fullyMaybe sc = fully sc . optional

-- | A utility for converting a 'ParserError' into a one line message:
--   @<line-nr>: <error-msg>@
showShortError :: ParserError -> String
showShortError pe = show (line + 1) <> ": " <> from msg
 where
  (((line, _), _), msg) = showErrorPos pe

-- | A utility for converting a 'ParserError' into a range and error message.
showErrorPos :: ParserError -> (((Int, Int), (Int, Int)), Text)
showErrorPos (ParseErrorBundle errs sourcePS) = ((minusOne start, minusOne end), from msg)
 where
  -- convert megaparsec source pos to starts at 0
  minusOne (x, y) = (x - 1, y - 1)

  -- get the first error position (ps) and line content (str)
  err = NE.head errs
  offset = case err of
    TrivialError x _ _ -> x
    FancyError x _ -> x
  (str, ps) = reachOffset offset sourcePS
  msg = parseErrorTextPretty err

  -- extract the error starting position
  start@(line, col) = getLineCol ps

  -- compute the ending position based on the word at starting position
  wordlength = case break (== ' ') . drop col <$> str of
    Just (word, _) -> length word + 1
    _ -> 0
  end = (line, col + wordlength)

getLineCol :: PosState a -> (Int, Int)
getLineCol ps = (line, col)
 where
  line = unPos $ sourceLine $ pstateSourcePos ps
  col = unPos $ sourceColumn $ pstateSourcePos ps

-- | Given a text, convert a range expressed as indices into the
--   text value to a range expressed in terms of (line number, column
--   number) pairs.
getLocRange :: Text -> (Int, Int) -> ((Int, Int), (Int, Int))
getLocRange code (locStart, locEnd) = (start, end)
 where
  start = getLocPos locStart
  end = getLocPos (dropWhiteSpace locEnd)

  -- remove trailing whitespace that got included by the lexer
  dropWhiteSpace offset
    | isWhiteSpace offset = dropWhiteSpace (offset - 1)
    | otherwise = offset
  isWhiteSpace offset =
    -- Megaparsec offset needs to be (-1) to start at 0
    T.index code (offset - 1) `elem` [' ', '\n', '\r', '\t']

  -- using megaparsec offset facility, compute the line/col
  getLocPos offset =
    let sourcePS =
          PosState
            { pstateInput = code
            , pstateOffset = 0
            , pstateSourcePos = Pos.initialPos ""
            , pstateTabWidth = Pos.defaultTabWidth
            , pstateLinePrefix = ""
            }
        (_, ps) = reachOffset offset sourcePS
     in getLineCol ps
