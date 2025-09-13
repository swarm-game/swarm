{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Validation of gameplay.
--
-- Facilities for running a game state until completion, checking for
-- any errors encountered.  This is not used for normal gameplay but
-- can be used by /e.g./ integration tests.
module Swarm.Game.Step.Validate where

import Control.Lens (use, (^.))
import Control.Monad.State (StateT, gets)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Swarm.Effect (runMetricIO, runTimeIO)
import Swarm.Game.Robot.Concrete (robotLog)
import Swarm.Game.State (GameState, messageInfo, robotInfo, winCondition)
import Swarm.Game.State.Robot (robotMap)
import Swarm.Game.State.Substate (WinCondition (..), WinStatus (..), messageQueue)
import Swarm.Game.Step (gameTick)
import Swarm.Game.Tick (TickNumber)
import Swarm.Log (logToText)

-- | Keep stepping a 'GameState' until completion, returning the
--   number of ticks taken if successful, or any bad error messages
--   encountered.
playUntilWin :: StateT GameState IO (Either (NE.NonEmpty T.Text) TickNumber)
playUntilWin = do
  w <- use winCondition
  b <- gets badErrorsInLogs
  case NE.nonEmpty b of
    Just badErrs -> return $ Left badErrs
    Nothing -> case w of
      WinConditions (Won _ ts) _ -> return $ Right ts
      _ -> runMetricIO (runTimeIO gameTick) >> playUntilWin

-- | Extract any bad error messages from robot logs or the global
--   message queue, where "bad" errors are either fatal errors or
--   ones referring to issues in the issue tracker.
badErrorsInLogs :: GameState -> [T.Text]
badErrorsInLogs g =
  concatMap
    (\r -> filter isBad (logToText $ r ^. robotLog))
    (g ^. robotInfo . robotMap)
    <> filter isBad (logToText $ g ^. messageInfo . messageQueue)
 where
  isBad m = "Fatal error:" `T.isInfixOf` m || "swarm/issues" `T.isInfixOf` m
