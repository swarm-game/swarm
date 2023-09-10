{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestNotification where

import Control.Lens (Getter, Ixed (ix), view, (&), (.~), (^.), (^?!))
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.CESK (TickNumber (..))
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Log
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil

testNotification :: GameState -> TestTree
testNotification gs =
  testGroup
    "Notifications"
    [ testCase "notifications at start" $ do
        assertBool "There should be no messages in queue" (null (gs ^. messageQueue))
        assertNew gs 0 "messages at game start" messageNotifications
        assertNew gs 0 "recipes at game start" availableRecipes
        assertNew gs 0 "commands at game start" availableCommands
    , testCase "new message after say" $ do
        gs' <- goodPlay "say \"Hello world!\""
        assertBool "There should be one message in queue" (length (gs' ^. messageQueue) == 1)
        assertNew gs' 1 "message" messageNotifications
    , testCase "two new messages after say twice" $ do
        gs' <- goodPlay "say \"Hello!\"; say \"Goodbye!\""
        assertBool "There should be two messages in queue" (length (gs' ^. messageQueue) == 2)
        assertNew gs' 2 "messages" messageNotifications
    , testCase "one new message and one old message" $ do
        gs' <- goodPlay "say \"Hello!\"; say \"Goodbye!\""
        assertEqual "There should be two messages in queue" [TickNumber 0, TickNumber 1] (view leTime <$> gs' ^. messageNotifications . notificationsContent)
        assertNew (gs' & lastSeenMessageTime .~ TickNumber 0) 1 "message" messageNotifications
    , testCase "new message after log" $ do
        gs' <- goodPlay "create \"logger\"; equip \"logger\"; log \"Hello world!\""
        let r = gs' ^?! robotMap . ix (-1)
        assertBool "There should be one log entry in robots log" (length (r ^. robotLog) == 1)
        assertEqual "The hypothetical robot should be in focus" (Just (r ^. robotID)) (view robotID <$> focusedRobot gs')
        assertEqual "There should be one log notification" [TickNumber 2] (view leTime <$> gs' ^. messageNotifications . notificationsContent)
        assertNew gs' 1 "message" messageNotifications
    , testCase "new message after build say" $ do
        gs' <- goodPlay "build {say \"Hello world!\"}; turn back; turn back;"
        assertBool "There should be one message in queue" (length (gs' ^. messageQueue) == 1)
        assertNew gs' 1 "message" messageNotifications
    , testCase "no new message after build log" $ do
        gs' <- goodPlay "build {log \"Hello world!\"}; turn back; turn back;"
        assertNew gs' 0 "message" messageNotifications
    ]
 where
  goodPlay :: Text -> IO GameState
  goodPlay t = do
    (e, g) <- play gs t
    either (assertFailure . T.unpack) pure e
    return g
  assertNew :: s -> Int -> String -> Getter s (Notifications a) -> Assertion
  assertNew g n what l =
    let c = g ^. l . notificationsCount
     in assertEqual ("There should be exactly " <> show n <> " new " <> what) n c
