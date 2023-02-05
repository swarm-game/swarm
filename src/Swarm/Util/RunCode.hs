module Swarm.Util.RunCode where

import Control.Arrow (left)
import Control.Monad.Except
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Data.Text.IO qualified as TIO
import Swarm.Language.Pipeline (ProcessedTerm, processTermEither)

data SolutionSource
  = ScenarioSuggested
  | PlayerAuthored

data CodeToRun = CodeToRun SolutionSource ProcessedTerm

getParsedInitialCode :: Maybe FilePath -> ExceptT Text IO (Maybe CodeToRun)
getParsedInitialCode toRun = do
  maybeRunScript <- ExceptT $ case toRun of
    Nothing -> return $ Right Nothing
    Just filepath -> do
      contents <- TIO.readFile filepath
      return $
        sequenceA $
          Just $
            left T.pack $
              processTermEither contents

  return $ CodeToRun PlayerAuthored <$> maybeRunScript
