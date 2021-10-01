{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as Text
import           Options.Applicative
import           Swarm.App                      ( appMain )
import           Swarm.Language.LSP             ( lspMain )
import           Swarm.Language.Pipeline        ( processTerm )
import           System.Exit

data CLI
  = Run
  | Format FilePath
  | LSP

cliParser :: Parser CLI
cliParser =
  subparser
      (  command "format" (info format (progDesc "Format a file"))
      <> command "lsp"    (info (pure LSP) (progDesc "Start the LSP"))
      )
    <|> pure Run
  where format = Format <$> strArgument (metavar "FILE")

cliInfo :: ParserInfo CLI
cliInfo = info (cliParser <**> helper) (fullDesc <> header "Swarm game")

-- | Utility function to validate and format swarm-lang code
formatFile :: FilePath -> Text -> IO ()
formatFile fp content = do
  case processTerm content of
    Right _ -> do
      Text.putStrLn content
      exitSuccess
    Left e -> do
      Text.putStrLn $ pack fp <> ":" <> e
      exitFailure

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    Run       -> appMain
    Format fp -> formatFile fp =<< Text.readFile fp
    LSP       -> lspMain
