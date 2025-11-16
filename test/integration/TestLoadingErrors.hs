{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test for runtime errors during loading.
module TestLoadingErrors (testNoLoadingErrors, checkNoRuntimeErrors) where

import Control.Lens (view, (^.))
import Control.Monad (forM_, when)
import Data.Text qualified as T
import Swarm.Game.State.Runtime (RuntimeState, eventLog)
import Swarm.Game.State.Substate (notificationsContent)
import Swarm.Log (LogEntry, Severity (..), leSeverity, leText)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase)

testNoLoadingErrors :: RuntimeState -> TestTree
testNoLoadingErrors r =
  testCase "Test runtime log does not contain errors" (checkNoRuntimeErrors r)

checkNoRuntimeErrors :: RuntimeState -> IO ()
checkNoRuntimeErrors r =
  forM_ (r ^. eventLog . notificationsContent) $ \e ->
    when (isError e) $
      assertFailure $
        show (e ^. leSeverity) <> " was produced during loading: " <> T.unpack (e ^. leText)

isError :: LogEntry -> Bool
isError = (>= Warning) . view leSeverity
