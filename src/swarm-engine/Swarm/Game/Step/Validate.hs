{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Validation of gameplay.
--
-- Facilities for running a game state until completion, checking for
-- any errors encountered.  This is not used for normal gameplay but
-- can be used by /e.g./ integration tests.
module Swarm.Game.Step.Validate (playUntilWin, badErrorsInLogs) where

import Control.Lens ((^.))
import Control.Monad.State.Strict qualified as S
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local
import Effectful.State.Static.Local qualified as E
import Swarm.Effect
import Swarm.Game.Robot.Concrete (robotLog)
import Swarm.Game.State (GameState, messageInfo, robotInfo, winCondition)
import Swarm.Game.State.Robot (robotMap)
import Swarm.Game.State.Substate (WinCondition (..), WinStatus (..), messageQueue)
import Swarm.Game.Step (gameTick)
import Swarm.Game.Tick (TickNumber)
import Swarm.Language.Cache (moduleCache)
import Swarm.Language.Module (Module)
import Swarm.Language.Syntax (Phase (Elaborated))
import Swarm.Language.Syntax.Import
import Swarm.Log (logToText)
import Swarm.Util.Lens

-- | Keep stepping a 'GameState' until completion, returning the
--   number of ticks taken if successful, or any bad error messages
--   encountered.
playUntilWin :: S.StateT GameState IO (Either (NE.NonEmpty T.Text) TickNumber)
playUntilWin = do
  gs <- S.get
  (result, gs') <- liftIO . runEff . runCacheIO moduleCache . runMetricIO . runTimeIO . E.runState gs $ playUntilWin'
  S.put gs'
  pure result

playUntilWin' ::
  ( State GameState :> es
  , Cache (ImportLoc Resolved) (Module Elaborated) :> es
  , Metric :> es
  , Time :> es
  ) =>
  Eff es (Either (NE.NonEmpty T.Text) TickNumber)
playUntilWin' = do
  w <- use winCondition
  b <- gets badErrorsInLogs
  case NE.nonEmpty b of
    Just badErrs -> pure $ Left badErrs
    Nothing -> case w of
      WinConditions (Won _ ts) _ -> pure $ Right ts
      _ -> gameTick >> playUntilWin'

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
