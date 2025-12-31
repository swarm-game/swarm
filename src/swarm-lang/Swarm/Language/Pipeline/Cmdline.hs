{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing + typechecking swarm-lang code at the command line.
module Swarm.Language.Pipeline.Cmdline where

import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Data.Text.IO qualified as T
import Swarm.Failure (SystemFailure)
import Swarm.Language.Pipeline (processSource)
import Swarm.Pretty (prettyText)
import Swarm.Util.InputSource (InputSource, getInput, inputSourceToMaybe)
import System.Exit (exitFailure)
import System.IO (stderr)

newtype CheckConfig = CheckConfig {checkInput :: InputSource}

-- | Validate swarm-lang code.
checkSwarmIO :: CheckConfig -> IO ()
checkSwarmIO (CheckConfig input) = do
  T.putStr $ "Checking " <> prettyText input <> " ... "
  mcontent <- getInput input
  case mcontent of
    Nothing -> T.hPutStrLn stderr $ "Could not read from " <> prettyText input
    Just content -> do
      res <- runM . runError @SystemFailure $ processSource (inputSourceToMaybe input) Nothing content
      case res of
        Right _ -> T.putStrLn "OK."
        Left err -> do
          T.putStrLn "error:"
          T.hPutStrLn stderr (prettyText err)
          exitFailure
