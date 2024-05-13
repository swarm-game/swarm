{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Formatting Swarm language code.
module Swarm.Language.Format where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.Text qualified as RT
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Pretty
import Swarm.Util ((?))
import System.Console.Terminal.Size qualified as Term
import System.Exit (exitFailure)
import System.IO (stderr)

type FormatWidth = Int

-- | From where should the input be taken?
data FormatInput = Stdin | InputFile FilePath

-- | Where should the formatted code be output?
data FormatOutput = Stdout | OutputFile FilePath | Inplace

getInput :: FormatInput -> IO Text
getInput Stdin = T.getContents
getInput (InputFile fp) = T.readFile fp

showInput :: FormatInput -> Text
showInput Stdin = "(input)"
showInput (InputFile fp) = T.pack fp

-- | Validate and format swarm-lang code.
formatSwarmIO :: FormatInput -> FormatOutput -> Maybe FormatWidth -> IO ()
formatSwarmIO input output mWidth = do
  content <- getInput input
  mWindowWidth <- (fmap . fmap) Term.width Term.size
  let w = mWidth <|> case output of Stdout -> mWindowWidth; _ -> Nothing
  case formatSwarm w content of
    Right fmt -> case output of
      Stdout -> T.putStrLn fmt
      OutputFile outFile -> T.writeFile outFile fmt
      Inplace -> case input of
        Stdin -> T.putStrLn fmt
        InputFile inFile -> T.writeFile inFile fmt
    Left e -> do
      T.hPutStrLn stderr $ showInput input <> ":" <> e
      exitFailure

formatSwarm :: Maybe FormatWidth -> Text -> Either Text Text
formatSwarm mWidth content = case readTerm content of
  Right Nothing -> Right ""
  Right (Just ast) ->
    let mkOpt w = LayoutOptions (AvailablePerLine w 1.0)
        opt = (mkOpt <$> mWidth) ? defaultLayoutOptions
     in Right . RT.renderStrict . layoutPretty opt $ ppr ast
  Left e -> Left e
