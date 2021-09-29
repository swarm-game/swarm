module Main where

import           Data.Text                      ( unpack )
import qualified Data.Text.IO                  as Text
import           Swarm.App
import           Swarm.Language.Parse
import           Swarm.Language.Pipeline
import           System.Environment             ( getArgs )
import           System.Exit

check :: FilePath -> IO ()
check fp = do
  content <- Text.readFile fp
  let err = case readTerm' content of
        Right term -> case processParsedTerm' mempty mempty term of
          Right _ -> Nothing
          Left  e -> Just $ " " <> unpack e
        Left e -> Just $ showShortError e
  case err of
    Nothing -> exitSuccess
    Just e  -> do
      putStrLn $ fp <> ":" <> e
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    []              -> appMain
    ["--check", fp] -> check fp
    _               -> putStrLn "usage: swarm [--check fp]"
