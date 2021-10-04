{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (
  Text,
  pack,
 )
import qualified Data.Text.IO as Text
import Options.Applicative
import Swarm.App (appMain)
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Pipeline (processTerm)
import System.Exit

data CLI
  = Run Int
  | Format Input
  | LSP

cliParser :: Parser CLI
cliParser =
  subparser
    ( command "format" (info (format <**> helper) (progDesc "Format a file"))
        <> command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
    )
    <|> Run <$> seed
 where
  format =
    (Format Stdin <$ switch (long "stdin" <> help "Read code from stdin"))
      <|> (Format . File <$> strArgument (metavar "FILE"))
  seed :: Parser Int
  seed = option auto (long "seed" <> short 's' <> value 0)

cliInfo :: ParserInfo CLI
cliInfo = info (cliParser <**> helper) (fullDesc <> header "Swarm game")

-- | Utility function to validate and format swarm-lang code
data Input = Stdin | File FilePath

getInput :: Input -> IO Text
getInput Stdin = Text.getContents
getInput (File fp) = Text.readFile fp

showInput :: Input -> Text
showInput Stdin = "(input)"
showInput (File fp) = pack fp

formatFile :: Input -> IO ()
formatFile input = do
  content <- getInput input
  case processTerm content of
    Right _ -> do
      Text.putStrLn content
      exitSuccess
    Left e -> do
      Text.putStrLn $ showInput input <> ":" <> e
      exitFailure

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    Run seed -> appMain seed
    Format fo -> formatFile fo
    LSP -> lspMain
