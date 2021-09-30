{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text.IO                  as Text
import           Swarm.App                      ( appMain )
import           Swarm.Language.LSP             ( lspMain )
import           Swarm.Language.Pipeline        ( processTerm )
import           System.Environment             ( getArgs )
import           System.Exit

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
  args <- getArgs
  case args of
    []               -> appMain
    ["--lsp"]        -> lspMain
    ["--format", fp] -> formatFile fp =<< Text.readFile fp
    _                -> putStrLn "usage: swarm [--format fp]"
