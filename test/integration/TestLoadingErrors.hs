{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test for runtime errors during loading.
module TestLoadingErrors (testNoLoadingErrors, checkNoRuntimeErrors) where

import Control.Lens (view, (^.))
import Control.Monad (unless)
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
checkNoRuntimeErrors r = do
  let errs = filter isError (r ^. eventLog . notificationsContent)
  unless (null errs)
    . assertFailure
    . unlines
    $ "Warnings or errors produced during loading:"
      : map (\e -> T.unpack (e ^. leText)) errs

isError :: LogEntry -> Bool
isError = (>= Warning) . view leSeverity
