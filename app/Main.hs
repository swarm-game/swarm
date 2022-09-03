{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable qualified
import Data.Text (Text, pack)
import Data.Text.IO qualified as Text
import Options.Applicative
import Swarm.App (appMain)
import Swarm.DocGen (EditorType (..), GenerateDocs (..), SheetType (..), generateDocs)
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Pipeline (processTerm)
import Swarm.Version
import Swarm.Web (defaultPort)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

data CLI
  = Run
      (Maybe Int) -- seed
      (Maybe FilePath) -- scenario
      (Maybe FilePath) -- file to run
      Bool -- cheat mode
      (Maybe Int) -- web port
  | Format Input
  | DocGen GenerateDocs
  | LSP
  | Version

cliParser :: Parser CLI
cliParser =
  subparser
    ( mconcat
        [ command "format" (info (format <**> helper) (progDesc "Format a file"))
        , command "generate" (info (DocGen <$> docgen <**> helper) (progDesc "Generate docs"))
        , command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
        , command "version" (info (pure Version) (progDesc "Get current and upstream version."))
        ]
    )
    <|> Run <$> seed <*> scenario <*> run <*> cheat <*> webPort
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
      , command "cheatsheet" (info (pure $ CheatSheet $ Just Commands) $ progDesc "Output nice Wiki tables")
      ]
  editor :: Parser (Maybe EditorType)
  editor =
    Data.Foldable.asum
      [ pure Nothing
      , Just VSCode <$ switch (long "code" <> help "Generate for the VS Code editor")
      , Just Emacs <$ switch (long "emacs" <> help "Generate for the Emacs editor")
      ]
  seed :: Parser (Maybe Int)
  seed = optional $ option auto (long "seed" <> short 's' <> metavar "INT" <> help "Seed to use for world generation")
  webPort :: Parser (Maybe Int)
  webPort =
    optional $
      option
        auto
        ( long "web"
            <> metavar "PORT"
            <> help ("Set the web service port (or disable it with 0). Default to " <> show defaultPort <> ".")
        )
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
    ( header ("Swarm game - " <> version <> commitInfo)
        <> progDesc "To play the game simply run without any command."
        <> fullDesc
    )

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

showVersion :: IO ()
showVersion = do
  putStrLn $ "Swarm game - " <> version <> commitInfo
  up <- getNewerReleaseVersion
  either (hPutStrLn stderr) (putStrLn . ("New upstream release: " <>)) up

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    Run seed scenario toRun cheat webPort -> appMain webPort seed scenario toRun cheat
    DocGen g -> generateDocs g
    Format fo -> formatFile fo
    LSP -> lspMain
    Version -> showVersion
