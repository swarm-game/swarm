{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestModel where

import Data.String (fromString)
import Data.Text (Text)
import Swarm.TUI.Model
import Test.Tasty
import Test.Tasty.HUnit

testModel :: TestTree
testModel =
  testGroup
    "TUI Model"
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
            [REPLEntry "()"]
            (getLatestREPLHistoryItems 5 (addREPLItem (REPLEntry "()") history0))
        )
    , testCase
        "latest repl lines after one input and output"
        ( assertEqual
            "get 5 history [0|1,1:int] --> [1,1:int]"
            [REPLEntry "1", REPLOutput "1:int"]
            (getLatestREPLHistoryItems 5 (addInOutInt 1 history0))
        )
    , testCase
        "latest repl lines after nine inputs and outputs"
        ( assertEqual
            "get 6 history [0|1,1:int .. 9,9:int] --> [7,7:int..9,9:int]"
            (concat [[REPLEntry (toT x), REPLOutput (toT x <> ":int")] | x <- [7 .. 9]])
            (getLatestREPLHistoryItems 6 (foldl (flip addInOutInt) history0 [1 .. 9]))
        )
    , testCase
        "latest repl after restart"
        ( assertEqual
            "get 5 history (restart [0|()]) --> []"
            []
            (getLatestREPLHistoryItems 5 (restartREPLHistory $ addREPLItem (REPLEntry "()") history0))
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
            "getText ([0,1,1:int]<=='') --> Just 1"
            (Just "1")
            (getCurrentItemText $ moveReplHistIndex Older "" (addInOutInt 1 history0))
        )
    , testCase
        "current item after move past same"
        ( assertEqual
            "getText ([0,1,1:int]<=='1') --> Just 0"
            (Just "0")
            (getCurrentItemText $ moveReplHistIndex Older "1" (addInOutInt 1 history0))
        )
    ]
 where
  history0 = newREPLHistory [REPLEntry "0"]
  toT :: Int -> Text
  toT = fromString . show
  addInOutInt :: Int -> REPLHistory -> REPLHistory
  addInOutInt i = addREPLItem (REPLOutput $ toT i <> ":int") . addREPLItem (REPLEntry $ toT i)
