{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing + typechecking swarm-lang code at the command line.
module Swarm.Language.Pipeline.Cmdline where

import Data.Text.IO qualified as T
import Swarm.Language.Pipeline (processTerm)
import Swarm.Pretty (prettyText)
import Swarm.Util.InputSource (InputSource, getInput)
import System.Exit (exitFailure)
import System.IO (stderr)

newtype CheckConfig = CheckConfig {checkInput :: InputSource}

-- | Validate swarm-lang code.
checkSwarmIO :: CheckConfig -> IO ()
checkSwarmIO (CheckConfig input) = do
  T.putStrLn $ "Checking " <> prettyText input <> " ..."
  mcontent <- getInput input
  case mcontent of
    Nothing -> T.hPutStrLn stderr $ "Could not read from " <> prettyText input
    Just content -> do
      case processTerm content of
        Right _ -> T.putStrLn "OK."
        Left err -> do
          T.putStrLn err
          exitFailure
