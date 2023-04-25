{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing and pretty-printing for keys (as in, keys on a keyboard)
-- and key combos.
module Swarm.Language.Key (
  KeyCombo,
  mkKeyCombo,
  parseKeyComboFull,
  parseKeyCombo,
  prettyKeyCombo,
  specialKeyNames,
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (asum)
import Data.List (sort, (\\))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics hiding (from)
import Graphics.Vty.Input.Events qualified as V
import Swarm.Language.Parse
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Witch (from)

------------------------------------------------------------
-- Parsing

-- | A keyboard input, represented as a key + modifiers.  Invariant:
--   the modifier list is always sorted.
data KeyCombo = KeyCombo V.Key [V.Modifier]
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

deriving instance FromJSON V.Key
deriving instance FromJSON V.Modifier
deriving instance ToJSON V.Key
deriving instance ToJSON V.Modifier

-- | Smart constructor for 'KeyCombo'.
mkKeyCombo :: [V.Modifier] -> V.Key -> KeyCombo
mkKeyCombo mods k = KeyCombo k (sort mods)

-- | Parse a key combo with nothing after it.
parseKeyComboFull :: Parser KeyCombo
parseKeyComboFull = parseKeyCombo <* eof

-- | Parse a key combo like "M-C-F5", "Down", or "C-x".
parseKeyCombo :: Parser KeyCombo
parseKeyCombo =
  mkKeyCombo <$> many (try (parseModifier <* char '-')) <*> parseKey

parseModifier :: Parser V.Modifier
parseModifier =
  V.MShift <$ string "S"
    <|> V.MCtrl <$ string "C"
    <|> V.MMeta <$ string "M"
    <|> V.MAlt <$ string "A"

parseKey :: Parser V.Key
parseKey =
  -- For an explanation of the 'reverse', see Note [Key names are not prefix-free]
  (asum . map specialKeyParser . reverse . S.toList $ specialKeyNames)
    <|> parseFunctionKey
    <|> parseCharKey

-- Note [Key names are not prefix-free]
--
-- The names of special keys are not prefix-free, and in particular
-- include 'Down', 'DownRight', 'DownLeft', and also 'Up', 'UpRight',
-- 'UpLeft'.  When we try to parse a particular name with 'string' it
-- will backtrack as long as the whole string is not consumed, which
-- means it's OK if key names share a common prefix, like Enter and
-- Esc.  However, when one key name is a prefix of another we have to
-- be careful of the order in which we try parsing them, and in
-- particular we must try parsing the longer one first. If we have
-- 'Up' come first and then 'UpLeft', for example, given the input
-- "UpLeft" the 'Up' would succeed, but then the entire parse would
-- fail since there is input left over.  If we simply reverse the list
-- of key names (which are sorted alphabetically), it guarantees that
-- longer names will come before names which are prefixes of them.

parseFunctionKey :: Parser V.Key
parseFunctionKey = V.KFun <$> try (char 'F' *> decimal)

parseCharKey :: Parser V.Key
parseCharKey = V.KChar <$> anySingle

specialKeyParser :: Text -> Parser V.Key
specialKeyParser t = read . ('K' :) . from @Text <$> string t

-- https://stackoverflow.com/questions/51848587/list-constructor-names-using-generics-in-haskell
specialKeyNames :: Set Text
specialKeyNames = S.fromList . map T.tail $ (names' @(Rep V.Key) \\ ["KChar", "KFun"])

class Names' (f :: * -> *) where
  names' :: [Text]
instance (Names' f) => Names' (M1 D t f) where
  names' = names' @f
instance (Names' f, Names' g) => Names' (f :+: g) where
  names' = names' @f ++ names' @g
instance (Constructor c) => Names' (C1 c f) where
  names' = [from @String (conName (undefined :: C1 c f g))]

------------------------------------------------------------
-- Pretty-printing

-- | Pretty-print a key combo, e.g. "C-M-F5".  Right inverse to
--   parseKeyCombo.  Left inverse up to reordering of modifiers.
prettyKeyCombo :: KeyCombo -> Text
prettyKeyCombo (KeyCombo k mods) = T.append (T.concat (map prettyModifier mods)) (prettyKey k)

prettyModifier :: V.Modifier -> Text
prettyModifier m = from @String [modifierChar m, '-']
 where
  modifierChar = \case
    V.MAlt -> 'A'
    V.MCtrl -> 'C'
    V.MMeta -> 'M'
    V.MShift -> 'S'

prettyKey :: V.Key -> Text
prettyKey =
  from @String . \case
    V.KChar c -> [c]
    V.KFun n -> 'F' : show n
    k -> tail (show k)
