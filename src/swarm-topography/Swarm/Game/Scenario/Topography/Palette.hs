{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Palette (
  SignpostableCell (..),
  StructureMarker (..),
  StructurePalette (..),
) where

import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Void (Void)
import GHC.Generics (Generic, Generically (..))
import Swarm.Game.Scenario.Topography.ProtoCell
import Swarm.Util (quote)
import Swarm.Util.Yaml
import Text.Megaparsec
import Text.Megaparsec.Char

data StructurePalette e = StructurePalette
  { paletteChars :: Set Char
  -- ^ Set of characters that can be used as special character
  --   entities that just look like themselves, for use in labelling
  --   things in the world with text
  , explicitChars :: Set Char
  -- ^ Set of characters that were explicitly given individual
  --   definitions (as opposed to just being part of paletteChars)
  , unPalette :: Map Char (SignpostableCell e)
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via Generically (StructurePalette e)

instance (FromJSONE e a) => FromJSONE e (StructurePalette a) where
  parseJSONE =
    withObjectE "palette" $ \km -> do
      -- Try parsing special 'chars' key
      chars <- case KM.lookup "chars" km of
        Nothing -> pure S.empty
        Just charsVal -> withTextE "charset" readCharSet charsVal

      m <- mapM parseJSONE (KM.delete "chars" km)
      -- We swap the tuples twice so we can traverse over the second
      -- element of the tuple in between.
      swappedPairs <- mapM (verifyChar . swap) $ M.toList $ KM.toMap m
      let pairs = map swap swappedPairs
      return . StructurePalette chars (S.fromList $ map fst pairs) . M.fromList $ pairs
   where
    verifyChar = traverse $ ensureSingleChar . K.toString
    ensureSingleChar [x] = return x
    ensureSingleChar x =
      fail $
        T.unpack $
          T.unwords
            [ "Palette entry is not a single character:"
            , quote $ T.pack x
            ]

------------------------------------------------------------
-- Parsing character sets

-- | Parse a character set, which consists of a sequence of items;
--   each item is either a single character, or a character range of
--   the form @x-y@ where @x@ and @y@ are characters.  For example,
--   @A-Z1-9!?@ corresponds to all uppercase letters, all nonzero
--   digits, and exclamation point or question mark.
readCharSet :: Text -> ParserE e (Set Char)
readCharSet t = case runParser parseCharSet "" t of
  Left err -> fail (errorBundlePretty err)
  Right s -> pure s

type Parser = Parsec Void Text

--   <charset> ::= <item>+
--   <item> ::= <single> | <range>
--   <single> ::= <char> (not '-')
--   <range> ::= <single> '-' <single>

parseCharSet :: Parser (Set Char)
parseCharSet = S.unions <$> many parseItem

parseItem :: Parser (Set Char)
parseItem = try parseRange <|> S.singleton <$> parseSingle

parseRange :: Parser (Set Char)
parseRange = mkRange <$> parseSingle <*> (char '-' *> parseSingle)

mkRange :: Char -> Char -> Set Char
mkRange x y = S.fromList [x .. y]

parseSingle :: Parser Char
parseSingle = anySingleBut '-' <?> "non-hyphen character"
