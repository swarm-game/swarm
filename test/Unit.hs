{-# LANGUAGE OverloadedStrings #-}

-- | Swarm unit tests
module Main where

import           Data.Text (Text)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Swarm.Language.Pipeline

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parser]

parser :: TestTree
parser =
  testGroup
    "Language"
    [ testCase "end semicolon #79" (valid "def a = 41 end def b = a + 1 end def c = b + 2 end")
    ]
  where
    valid = flip process ""
    process :: Text -> Text -> Assertion
    process code expect = case processTerm code of
      Left e | e == expect   -> pure ()
             | otherwise     -> error $ "Unexpected failure: " <> show e
      Right _ | expect == "" -> pure ()
              | otherwise    -> error "Unexpected success"
