{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import Lib

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Test structure assembly"
      [ mkGroup
          "Black and white"
          [ "circle-and-crosses"
          , "checkerboard"
          ]
      , mkGroup
          "Color"
          [ "rainbow"
          ]
      ]
 where
  doTest stem =
    testCase (unwords ["Image equality:", stem]) $
      compareToReferenceImage stem

  mkGroup title members =
    testGroup title $
      map
        doTest
        members
