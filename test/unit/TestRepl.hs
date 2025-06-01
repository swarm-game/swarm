{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm REPL unit tests
module TestRepl where

import Data.String (fromString)
import Data.Text (Text)
import Swarm.Game.Tick (TickNumber (..))
import Swarm.TUI.Model.Repl
import Test.Tasty
import Test.Tasty.HUnit

testRepl :: TestTree
testRepl =
  testGroup
    "TUI REPL"
    [ testCase
        "latest repl lines at start"
        ( assertEqual
            "get 5 history [0] --> []"
            []
            (getLatestREPLHistoryItems 5 history0)
        )
    , testCase
        "latest repl lines after one input"
        ( assertEqual
            "get 5 history [0|()] --> [()]"
            [mkREPLSubmission "()"]
            (getLatestREPLHistoryItems 5 (addREPLItem (mkREPLSubmission "()") history0))
        )
    , testCase
        "latest repl lines after one input and output"
        ( assertEqual
            "get 5 history [0|1,1:Int] --> [1,1:Int]"
            [mkREPLSubmission "1", mkREPLOutput "1:Int"]
            (getLatestREPLHistoryItems 5 (addInOutInt 1 history0))
        )
    , testCase
        "latest repl lines after nine inputs and outputs"
        ( assertEqual
            "get 6 history [0|1,1:Int .. 9,9:Int] --> [7,7:Int..9,9:Int]"
            (concat [[mkREPLSubmission (toT x), mkREPLOutput (toT x <> ":Int")] | x <- [7 .. 9]])
            (getLatestREPLHistoryItems 6 (foldl (flip addInOutInt) history0 [1 .. 9]))
        )
    , testCase
        "latest repl after restart"
        ( assertEqual
            "get 5 history (restart [0|()]) --> []"
            []
            (getLatestREPLHistoryItems 5 (restartREPLHistory $ addREPLItem (mkREPLSubmission "()") history0))
        )
    , testCase
        "current item at start"
        (assertEqual "getText [0] --> Nothing" (getCurrentItemText history0) Nothing)
    , testCase
        "current item after move to older"
        ( assertEqual
            "getText ([0]<=='') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "" history0)
        )
    , testCase
        "current item after move to newer"
        ( assertEqual
            "getText ([0]==>'') --> Nothing"
            Nothing
            (getCurrentItemText $ moveReplHistIndex Newer "" history0)
        )
    , testCase
        "current item after move past output"
        ( assertEqual
            "getText ([0,1,1:Int]<=='') --> Just 1"
            (Just "1")
            (getCurrentItemText $ moveReplHistIndex Older "" (addInOutInt 1 history0))
        )
    , testCase
        "current item after move past same"
        ( assertEqual
            "getText ([0,1,1:Int]<=='1') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "1" (addInOutInt 1 history0))
        )
    ]
 where
  history0 = newREPLHistory [mkREPLSubmission "0"]
  toT :: Int -> Text
  toT = fromString . show
  addInOutInt :: Int -> REPLHistory -> REPLHistory
  addInOutInt i = addREPLItem (mkREPLOutput $ toT i <> ":Int") . addREPLItem (mkREPLSubmission $ toT i)

mkREPLSubmission :: Text -> REPLHistItem
mkREPLSubmission = REPLHistItem (REPLEntry Submitted) (TickNumber 0)

mkREPLOutput :: Text -> REPLHistItem
mkREPLOutput = REPLHistItem REPLOutput (TickNumber 0)
