{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Representation of documents + layout as token streams.
module Swarm.Text.Markdown.Token where

import Control.Arrow ((***))
import Control.Lens (both, over)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chop)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts qualified (IsString (..))
import Swarm.Text.Markdown.Document (TxtAttr)

------------------------------------------------------------
-- Tokens
------------------------------------------------------------

-- | At different phases we have different types of tokens available.
--   During the layout phase, we have various types of special tokens
--   which we translate away by the output phase.
data TokenPhase = Layout | Output deriving (Eq, Ord, Show)

type Token = Token' Layout
type OutputToken = Token' Output

-- | Tokens in a stream that can be easily converted to text or brick widgets.
data Token' :: TokenPhase -> Type where
  -- | Basic text token.
  TextToken :: Text -> Token' p
  -- | Zero-width token to indicate that an attribute applies from now until a matching pop.
  PushAttr :: TxtAttr -> Token' p
  -- | Zero-width token to indicate that the matching attribute no longer applies.
  PopAttr :: Token' p
  -- | Line break.
  Newline :: Token' p
  -- | Paragraph break.
  Para :: Token' p
  -- | A "soft" space which is used only to separate other tokens.
  --   It should be displayed as a single space in between tokens,
  --   or discarded if it falls at the beginning or end of a line.
  SoftSpace :: Token' Layout
  -- | A "hard" or required space which must be displayed.  The Int
  --   is the number of consecutive space characters.  For example,
  --   a hard space may be generated at the beginning of a line in a
  --   code block, since the indentation is semantically meaningful
  --   and should neither be discarded nor turned into a single
  --   space.
  HardSpace :: Int -> Token' Layout
  -- | Several primitive tokens glued together, considered as an
  --   unbreakable group for the purposes of layout, but still kept
  --   separate so we can style or render them independently.
  Glue :: NonEmpty Token -> Token' Layout
  -- | Empty token.  Takes no space and produces no output.
  EmptyToken :: Token' Layout

deriving instance Eq (Token' p)
deriving instance Show (Token' p)

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | The width of a token is the amount of horizontal space it takes
--   up on a line.  Hence e.g. the width of a Newline token is
--   considered to be 0.
tokenWidth :: Token' p -> Int
tokenWidth = \case
  TextToken t -> T.length t
  PushAttr _ -> 0
  PopAttr -> 0
  Newline -> 0
  Para -> 0
  SoftSpace -> 1
  HardSpace n -> n
  Glue ts -> sum (fmap tokenWidth ts)
  EmptyToken -> 0

instance GHC.Exts.IsString (Token' p) where
  fromString = TextToken . T.pack

-- | Check if a token is a text token, extracting its text if so.
getTokenText :: Token' p -> Maybe Text
getTokenText = \case
  TextToken t -> Just t
  _ -> Nothing

------------------------------------------------------------
-- Token gluing
------------------------------------------------------------

-- | Glue a list of tokens into a single token appropriately.
glue :: [Token] -> Token
glue [] = EmptyToken
glue [t] = t
glue (t : ts) = Glue (t :| ts)

-- | Some tokens are "sticky" and like to stick to the other tokens to
--   their left or right, or both.  Neutral tokens are not sticky on
--   their own, but can have other tokens stick to them.  Nothing can
--   stick to nonstick tokens.
data Stickiness = StickyL | StickyR | StickyLR | Neutral | Nonstick deriving (Eq, Ord, Show)

-- | The "stickiness" of a token, i.e. whether it prefers to stick to
--   tokens on its left or right.
stickiness :: Token -> Stickiness
stickiness = \case
  TextToken t
    | t `elem` T.words "( [ {" -> StickyR
    | t `elem` T.words ". , ; : ? ! ) ] } - -- --- /" -> StickyL
    | t `elem` T.words "\" '" -> StickyLR
    | otherwise -> Neutral
  -- make push sticky so it won't be emitted by itself at the end of a line
  PushAttr _ -> StickyR
  PopAttr -> StickyL
  Newline -> Neutral
  Para -> Neutral
  -- Soft spaces are nonstick, so that e.g. things sticky in both
  -- directions (such as quote marks) stick to a nonspace thing next
  -- to them but not a space.
  SoftSpace -> Nonstick
  HardSpace {} -> StickyR
  Glue {} -> Neutral
  EmptyToken -> Neutral

-- | Preprocess a token stream by gluing together any sticky tokens.
glueTokens :: [Token] -> [Token]
glueTokens = chop (first glue . go1)
 where
  -- Get a first token.  If nonstick, just emit it.  Otherwise, start
  -- looking for sticky tokens: consecutive right-sticky tokens if the
  -- first token was right-sticky, or left-sticky otherwise.
  go1, goR, goL :: [Token] -> ([Token], [Token])
  go1 [] = ([], [])
  go1 (t : ts)
    | stickiness t == Nonstick = ([t], ts)
    | otherwise = first (t :) $ (if stickiness t `elem` [StickyR, StickyLR] then goR else goL) ts

  -- Look for consecutive right-sticky tokens...
  goR [] = ([], [])
  goR (t : ts)
    -- Accumulate right-sticky tokens and keep looking for more
    | stickiness t `elem` [StickyR, StickyLR] = first (t :) (goR ts)
    -- Stop if we encounter a nonstick token
    | stickiness t == Nonstick = ([], t : ts)
    -- Otherwise, switch to looking for left-sticky tokens
    | otherwise = first (t :) (goL ts)

  -- ...and then consecutive left-sticky tokens.
  goL [] = ([], [])
  goL (t : ts)
    -- Accumulate left-sticky tokens and keep looking for more
    | stickiness t == StickyL = first (t :) (goL ts)
    -- If we see a LR-sticky token, switch back to looking for right-sticky tokens
    | stickiness t == StickyLR = first (t :) (goR ts)
    -- Otherwise, stop
    | otherwise = ([], t : ts)

------------------------------------------------------------
-- Token splitting
------------------------------------------------------------

-- | Split a token into two, such that the first is no longer than the
--   specified length.
splitTokenAt :: Int -> Token -> (Token, Token)
splitTokenAt w = \case
  TextToken t -> over both mkNE (T.splitAt w t)
  PushAttr a -> (PushAttr a, EmptyToken)
  PopAttr -> (PopAttr, EmptyToken)
  Newline -> (Newline, EmptyToken)
  Para -> (Para, EmptyToken)
  SoftSpace -> (SoftSpace, EmptyToken)
  HardSpace n -> (HardSpace (min n w), if min n w == n then EmptyToken else HardSpace (n - min n w))
  Glue ts -> over both glue (splitTokenListAt w (NE.toList ts))
  EmptyToken -> (EmptyToken, EmptyToken)
 where
  mkNE t
    | T.null t = EmptyToken
    | otherwise = TextToken t

  splitTokenListAt :: Int -> [Token] -> ([Token], [Token])
  splitTokenListAt _ [] = ([], [])
  splitTokenListAt n (t : ts) = case compare (tokenWidth t) n of
    LT -> first (t :) (splitTokenListAt (n - tokenWidth t) ts)
    EQ -> ([t], ts)
    GT -> ((: []) *** (: ts)) (splitTokenAt n t)
