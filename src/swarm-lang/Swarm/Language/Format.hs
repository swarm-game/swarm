{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Formatting Swarm language code.
module Swarm.Language.Format where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~))
import Data.Foldable (foldl')
import Data.Set qualified as S
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.Text qualified as RT
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (LanguageVersion (..), defaultParserConfig, languageVersion)
import Swarm.Language.Parser.QQ (astQ)
import Swarm.Language.Syntax
import Swarm.Pretty (ppr)
import Swarm.Util (Encoding (..), (?), readFileMayT, writeFileT)
import System.Console.Terminal.Size qualified as Term
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Megaparsec.Error (errorBundlePretty)
import Witch (into)
import Prelude hiding (Foldable (..))

-- | From where should the input be taken?
data FormatInput = Stdin | InputFile FilePath

getInput :: FormatInput -> IO (Maybe Text)
getInput Stdin = Just <$> T.getContents
getInput (InputFile fp) = readFileMayT SystemLocale fp

showInput :: FormatInput -> Text
showInput Stdin = "(input)"
showInput (InputFile fp) = T.pack fp

-- | Where should the formatted code be output?
data FormatOutput = Stdout | OutputFile FilePath | Inplace

type FormatWidth = Int

data FormatConfig = FormatConfig
  { formatInput :: FormatInput
  , formatOutput :: FormatOutput
  , formatWidth :: Maybe FormatWidth
  , formatLanguageVersion :: LanguageVersion
  }

-- | Validate and format swarm-lang code.
formatSwarmIO :: FormatConfig -> IO ()
formatSwarmIO cfg@(FormatConfig input output mWidth _) = do
  mcontent <- getInput input
  case mcontent of
    Nothing -> T.hPutStrLn stderr $ "Could not read from " <> showInput input
    Just content -> do
      mWindowWidth <- (fmap . fmap) Term.width Term.size
      let w = mWidth <|> case output of Stdout -> mWindowWidth; _ -> Nothing
      case formatSwarm cfg {formatWidth = w} content of
        Right fmt -> case output of
          Stdout -> T.putStrLn fmt
          OutputFile outFile -> writeFileT SystemLocale outFile fmt
          Inplace -> case input of
            Stdin -> T.putStrLn fmt
            InputFile inFile -> writeFileT SystemLocale inFile fmt
        Left e -> do
          T.hPutStrLn stderr $ showInput input <> ":" <> e
          exitFailure

formatSwarm :: FormatConfig -> Text -> Either Text Text
formatSwarm (FormatConfig _ _ mWidth ver) content = case readTerm' cfg content of
  Right Nothing -> Right ""
  Right (Just ast) ->
    let ast' = case ver of
          SwarmLang0_6 -> addProjections_v7 ast
          _ -> ast
        mkOpt w = LayoutOptions (AvailablePerLine w 1.0)
        opt = (mkOpt <$> mWidth) ? defaultLayoutOptions
     in Right . RT.renderStrict . layoutPretty opt $ ppr ast'
  Left e -> Left (into @Text $ errorBundlePretty e)
 where
  cfg = defaultParserConfig & languageVersion .~ ver

-- | Version 0.6 of Swarm had keywords 'fst' and 'snd'; version 0.7
--   has 'match' instead.  Add definitions of 'fst' and 'snd' if the AST
--   contains free variables with those names.
addProjections_v7 :: Syntax -> Syntax
addProjections_v7 ast = foldl' addDefn ast (reverse $ S.toList freeProjs)
 where
  -- Any free occurrences of 'fst' or 'snd'?
  freeProjs = setOf freeVarsV ast `S.intersection` S.fromList ["fst", "snd"]

  addDefn :: Syntax -> Var -> Syntax
  addDefn rest = \case
    "fst" -> [astQ| def fst = \p. match p (\a. \_. a) end; $syn:rest |]
    "snd" -> [astQ| def snd = \p. match p (\_. \b. b) end; $syn:rest |]
    _ -> rest
