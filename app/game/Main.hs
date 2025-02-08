{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main (main) where

import Control.Monad (when)
import Data.Foldable qualified
import Data.List.Extra (enumerate)
import Data.Set qualified as Set
import Data.Text.IO qualified as T
import GitHash (GitInfo, giBranch, giHash, tGitInfoCwdTry)
import Options.Applicative
import Options.Applicative.Help hiding (color, fullDesc)
import Swarm.App (appMain)
import Swarm.Language.Format
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Parser.Core (LanguageVersion (..))
import Swarm.ResourceLoading (getSwarmConfigIniFile)
import Swarm.TUI.Model (AppOpts (..), ColorMode (..))
import Swarm.TUI.Model.DebugOption
import Swarm.TUI.Model.KeyBindings (KeybindingPrint (..), showKeybindings)
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
  | -- | Print list of bindings, optionally initializing the INI configuration file.
    ListKeybinding Bool KeybindingPrint
  | Format FormatConfig
  | LSP
  | Version

cliParser :: Parser CLI
cliParser =
  subparser
    ( mconcat
        [ command "run" (info (Run <$> appOpts <**> helper) (progDesc "Run the Swarm game (default)"))
        , command "format" (info (Format <$> parseFormat) (progDesc "Format a file"))
        , command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
        , command "version" (info (pure Version) (progDesc "Get current and upstream version."))
        , command "keybindings" (info (ListKeybinding <$> initKeybindingConfig <*> printKeyMode <**> helper) (progDesc "List the keybindings"))
        ]
    )
    <|> Run <$> appOpts
 where
  appOpts :: Parser AppOpts
  appOpts = do
    let repoGitInfo = gitInfo
    userSeed <- seed
    userScenario <- scenario
    scriptToRun <- run
    pausedAtStart <- paused
    autoPlay <- autoplay
    autoShowObjectives <- not <$> hideGoal
    speed <- speedFactor
    debugOptions <- debug
    cheatMode <- cheat
    colorMode <- color
    userWebPort <- webPort
    -- ApplicativeDo does not give Monad powers, so cheat is added here
    return $ addToDebug cheatMode $ AppOpts {..}

  addToDebug :: Set.Set DebugOption -> AppOpts -> AppOpts
  addToDebug cheatMode ao = ao {debugOptions = cheatMode `Set.union` debugOptions ao}

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
  langVer = flag SwarmLangLatest SwarmLang0_6 (long "v0.6" <> help "Read (& convert) code from Swarm version 0.6")

  printKeyMode :: Parser KeybindingPrint
  printKeyMode =
    flag' IniPrint (long "ini" <> help "Print in INI format")
      <|> flag' MarkdownPrint (long "markdown" <> help "Print in Markdown table format")
      <|> pure TextPrint

  initKeybindingConfig :: Parser Bool
  initKeybindingConfig = switch (short 'i' <> long "init" <> help "Initialise the keybindings configuration file")

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
  paused :: Parser Bool
  paused = switch (long "paused" <> short 'p' <> help "Pause the game at start.")
  autoplay :: Parser Bool
  autoplay = switch (long "autoplay" <> short 'a' <> help "Automatically run the solution defined in the scenario, if there is one. Mutually exclusive with --run.")
  hideGoal :: Parser Bool
  hideGoal = switch (long "hide-goal" <> help "Do not show goal modal window that pauses the game.")
  speedFactor :: Parser Int
  speedFactor = option auto (long "speed" <> short 'm' <> metavar "N" <> value defaultInitLgTicksPerSecond <> help speedFactorHelp)
  speedFactorHelp =
    unwords
      [ "Initial game speed."
      , "Counted as 2^N ticks per second, default is"
      , show defaultInitLgTicksPerSecond
      , "meaning"
      , show $ 2 ** fromIntegral @Int @Double defaultInitLgTicksPerSecond
      , "t/s."
      , "(Negative values are allowed, e.g. -3 means 1 tick per 8 sec.)"
      ]

  debug :: Parser (Set.Set DebugOption)
  debug = Set.unions <$> many debugOption
  debugOption :: Parser (Set.Set DebugOption)
  debugOption =
    option
      debugOptionList
      ( long "debug"
          <> short 'd'
          <> metavar "OPTS"
          <> hidden
          <> helpDoc debugOptionHelp
          <> completeWith debugCompletions
      )
  debugCompletions = "all" : map debugOptionName enumerate
  debugOptionList :: ReadM (Set.Set DebugOption)
  debugOptionList = eitherReader $ \case
    "all" -> pure $ Set.fromAscList enumerate
    opts -> Set.fromList <$> readDebugOptionList opts
  debugOptionHelp :: Maybe Doc
  debugOptionHelp =
    Just . nest 2 . vsep $
      "Use 'all' or a comma separated list of options:"
        : [ fillBreak 20 ("*" <+> pretty name) <+> pretty desc
          | o <- enumerate
          , let name = debugOptionName o
          , let desc = debugOptionDescription o
          ]
  cheat :: Parser (Set.Set DebugOption)
  cheat = flag mempty (Set.singleton ToggleCreative) (long "cheat" <> short 'x' <> helpDoc (Just cheatHelp))
  cheatHelp =
    "Enable cheat mode."
      <+> pretty ("This is an alias for --debug=" <> debugOptionName ToggleCreative)

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

printKeybindings :: Bool -> KeybindingPrint -> IO ()
printKeybindings initialize p = do
  kb <- showKeybindings p
  T.putStrLn kb
  (iniExists, ini) <- getSwarmConfigIniFile initialize
  when initialize $ do
    kbi <- showKeybindings IniPrint
    T.writeFile ini kbi
  let iniState
        | iniExists && initialize = "has been updated"
        | iniExists = "is"
        | initialize = "has been created"
        | otherwise = "can be created (--init)"
  putStrLn $ replicate 80 '-'
  putStrLn $ "The configuration file " <> iniState <> " at:"
  putStrLn ini

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    Run opts -> appMain opts
    ListKeybinding initialize p -> printKeybindings initialize p
    Format cfg -> formatSwarmIO cfg
    LSP -> lspMain
    Version -> showVersion
