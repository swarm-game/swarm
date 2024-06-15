{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import Lib

main :: IO ()
main = do
  defaultMain
    $ testGroup
      "Test structure assembly"
    $ map
      doTest
      [ "circle-and-crosses"
      , "checkerboard"
      ]
 where
  doTest stem = testCase (unwords ["Image equality:", stem]) $ compareToReferenceImage stem
