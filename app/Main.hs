{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Foldable qualified
import Data.Text (Text, pack)
import Data.Text.IO qualified as Text
import GitHash (giBranch, giHash, tGitInfoCwdTry)
import Options.Applicative
import Swarm.App (appMain)
import Swarm.DocGen (EditorType (..), GenerateDocs (..), generateDocs)
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Pipeline (processTerm)
import System.Exit (exitFailure, exitSuccess)

data CLI
  = Run
      (Maybe Int) -- seed
      (Maybe FilePath) -- scenario
      (Maybe FilePath) -- file to run
      Bool -- cheat mode
  | Format Input
  | DocGen GenerateDocs
  | LSP

cliParser :: Parser CLI
cliParser =
  subparser
    ( mconcat
        [ command "format" (info (format <**> helper) (progDesc "Format a file"))
        , command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
        , command "generate" (info (DocGen <$> docgen <**> helper) (progDesc "Generate docs"))
        ]
    )
    <|> Run <$> seed <*> scenario <*> run <*> cheat
 where
  format :: Parser CLI
  format =
    (Format Stdin <$ switch (long "stdin" <> help "Read code from stdin"))
      <|> (Format . File <$> strArgument (metavar "FILE"))
  docgen :: Parser GenerateDocs
  docgen =
    subparser . mconcat $
      [ command "recipes" (info (pure RecipeGraph) $ progDesc "Output graphviz dotfile of entity dependencies based on recipes")
      , command "editors" (info (EditorKeywords <$> editor <**> helper) $ progDesc "Output editor keywords")
      ]
  editor :: Parser (Maybe EditorType)
  editor =
    Data.Foldable.asum
      [ pure Nothing
      , Just VSCode <$ switch (long "code" <> help "Generate for the VS Code editor")
      , Just EMacs <$ switch (long "emacs" <> help "Generate for the EMacs editor")
      ]
  seed :: Parser (Maybe Int)
  seed = optional $ option auto (long "seed" <> short 's' <> metavar "INT" <> help "Seed to use for world generation")
  scenario :: Parser (Maybe String)
  scenario = optional $ strOption (long "scenario" <> short 'c' <> metavar "FILE" <> help "Name of a scenario to load")
  run :: Parser (Maybe String)
  run = optional $ strOption (long "run" <> short 'r' <> metavar "FILE" <> help "Run the commands in a file at startup")
  cheat :: Parser Bool
  cheat = switch (long "cheat" <> short 'x' <> help "Enable cheat mode")

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
    Run seed scenario toRun cheat -> appMain seed scenario toRun cheat
    Format fo -> formatFile fo
    DocGen g -> generateDocs g
    LSP -> lspMain
