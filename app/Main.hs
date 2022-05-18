{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Text (
  Text,
  pack,
 )
import qualified Data.Text.IO as Text
import GitHash
import Options.Applicative
import Swarm.App (appMain)
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Pipeline (processTerm)
import System.Exit

data CLI
  = -- seed, challenge file, run file
    Run (Maybe Int) (Maybe FilePath) (Maybe FilePath)
  | Format Input
  | LSP

cliParser :: Parser CLI
cliParser =
  subparser
    ( command "format" (info (format <**> helper) (progDesc "Format a file"))
        <> command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
    )
    <|> Run <$> seed <*> challenge <*> run
 where
  format :: Parser CLI
  format =
    (Format Stdin <$ switch (long "stdin" <> help "Read code from stdin"))
      <|> (Format . File <$> strArgument (metavar "FILE"))
  seed :: Parser (Maybe Int)
  seed = optional $ option auto (long "seed" <> short 's' <> metavar "INT" <> help "Seed to use for world generation")
  challenge :: Parser (Maybe String)
  challenge = optional $ strOption (long "scenario" <> short 'c' <> metavar "FILE" <> help "Name of a scenario to load")
  run :: Parser (Maybe String)
  run = optional $ strOption (long "run" <> short 'r' <> metavar "FILE" <> help "Run the commands in a file at startup")

cliInfo :: ParserInfo CLI
cliInfo =
  info
    (cliParser <**> helper)
    ( header ("Swarm game - pre-alpha version" <> commitInfo)
        <> progDesc "To play the game simply run without any command."
        <> fullDesc
    )
 where
  mgit = $$tGitInfoCwdTry
  commitInfo = case mgit of
    Left _ -> ""
    Right git -> " (" <> giBranch git <> "@" <> take 10 (giHash git) <> ")"

data Input = Stdin | File FilePath

getInput :: Input -> IO Text
getInput Stdin = Text.getContents
getInput (File fp) = Text.readFile fp

showInput :: Input -> Text
showInput Stdin = "(input)"
showInput (File fp) = pack fp

-- | Utility function to validate and format swarm-lang code
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
    Run seed challenge toRun -> appMain seed challenge toRun
    Format fo -> formatFile fo
    LSP -> lspMain
