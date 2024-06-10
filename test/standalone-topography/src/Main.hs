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
      [ testCase "Image equality" compareToReferenceImage
      ]
