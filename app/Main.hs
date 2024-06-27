{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Data.Foldable qualified
import Data.Text.IO qualified as T
import GitHash (GitInfo, giBranch, giHash, tGitInfoCwdTry)
import Options.Applicative
import Swarm.App (appMain)
import Swarm.Game.ResourceLoading (getSwarmConfigIniFile)
import Swarm.Language.Format
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Parser.Core (LanguageVersion (..))
import Swarm.TUI.Model (AppOpts (..), ColorMode (..))
import Swarm.TUI.Model.StateUpdate (KeybindingPrint (..), showKeybindings)
import Swarm.TUI.Model.UI (defaultInitLgTicksPerSecond)
import Swarm.Version
import Swarm.Web (defaultPort)
import System.IO (hPrint, stderr)
import Text.Read (readMaybe)

gitInfo :: Maybe GitInfo
gitInfo = either (const Nothing) Just $$tGitInfoCwdTry

commitInfo :: String
commitInfo = case gitInfo of
  Nothing -> ""
  Just git -> " (" <> giBranch git <> "@" <> take 10 (giHash git) <> ")"

data CLI
  = Run AppOpts
  | ListKeybinding KeybindingPrint
  | Format FormatConfig
  | LSP
  | Version

cliParser :: Parser CLI
cliParser =
  subparser
    ( mconcat
        [ command "format" (info (Format <$> parseFormat) (progDesc "Format a file"))
        , command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
        , command "version" (info (pure Version) (progDesc "Get current and upstream version."))
        , command "keybindings" (info (ListKeybinding <$> printKeyMode) (progDesc "List the keybindings"))
        ]
    )
    <|> Run
      <$> ( AppOpts
              <$> seed
              <*> scenario
              <*> run
              <*> autoplay
              <*> speedFactor
              <*> cheat
              <*> color
              <*> webPort
              <*> pure gitInfo
          )
 where
  input :: Parser FormatInput
  input =
    flag' Stdin (long "stdin" <> help "Read code from stdin")
      <|> (InputFile <$> strArgument (metavar "FILE"))

  output :: Parser FormatOutput
  output =
    flag Stdout Stdout (long "stdout" <> help "Write formatted code to stdout (default)")
      <|> (OutputFile <$> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Write formatted code to an output file"))
      <|> flag' Inplace (long "inplace" <> short 'i' <> help "Format file in place")

  widthOpt :: Parser FormatWidth
  widthOpt = option auto (long "width" <> metavar "COLUMNS" <> help "Use layout with maximum width")

  langVer :: Parser LanguageVersion
  langVer = flag SwarmLangLatest SwarmLang0_5 (long "v0.5" <> help "Read (& convert) code from Swarm version 0.5")

  printKeyMode :: Parser KeybindingPrint
  printKeyMode = flag MarkdownPrint TextPrint (long "markdown" <> help "Print in markdown table format.")

  parseFormat :: Parser FormatConfig
  parseFormat = FormatConfig <$> input <*> output <*> optional widthOpt <*> langVer <**> helper

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
  scenario = optional $ strOption (long "scenario" <> short 'i' <> metavar "FILE" <> help "Name of an input scenario to load")
  run :: Parser (Maybe String)
  run = optional $ strOption (long "run" <> short 'r' <> metavar "FILE" <> help "Run the commands in a file at startup")
  autoplay :: Parser Bool
  autoplay = switch (long "autoplay" <> short 'a' <> help "Automatically run the solution defined in the scenario, if there is one. Mutually exclusive with --run.")
  speedFactor :: Parser Int
  speedFactor = option auto (long "speed" <> short 'm' <> value defaultInitLgTicksPerSecond <> help "Initial game speed multiplier")
  cheat :: Parser Bool
  cheat = switch (long "cheat" <> short 'x' <> help "Enable cheat mode. This allows toggling Creative Mode with Ctrl+v and unlocks \"Testing\" scenarios in the menu.")
  color :: Parser (Maybe ColorMode)
  color = optional $ option colorModeParser (long "color" <> short 'c' <> metavar "MODE" <> help "Use none/8/16/full color mode.")
  colorModeParser =
    Data.Foldable.asum
      [ ColorMode8 <$ text "8"
      , ColorMode16 <$ text "16"
      , ColorMode240 <$> maybeReader (\case ('2' : '4' : '0' : '_' : w) -> readMaybe w; _ -> Nothing)
      , FullColor <$ text "full"
      , NoColor <$ text "none"
      ]
  text t = maybeReader (\x -> if x == t then Just x else Nothing)

cliInfo :: ParserInfo CLI
cliInfo =
  info
    (cliParser <**> helper)
    ( header ("Swarm game - " <> version <> commitInfo)
        <> progDesc "To play the game simply run without any command."
        <> fullDesc
    )

showVersion :: IO ()
showVersion = do
  putStrLn $ "Swarm game - " <> version <> commitInfo
  up <- getNewerReleaseVersion gitInfo
  either (hPrint stderr) (putStrLn . ("New upstream release: " <>)) up

printKeybindings :: KeybindingPrint -> IO ()
printKeybindings p = do
  kb <- showKeybindings p
  T.putStrLn kb
  (iniExists, ini) <- getSwarmConfigIniFile
  let iniState = if iniExists then "is" else "can be created"
  putStrLn $ "The configuration file " <> iniState <> " at:"
  putStrLn ini

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    Run opts -> appMain opts
    ListKeybinding p -> printKeybindings p
    Format cfg -> formatSwarmIO cfg
    LSP -> lspMain
    Version -> showVersion
